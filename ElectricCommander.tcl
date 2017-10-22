
if { $tcl_platform(machine) eq "x86_64" } {
    set bitness "64"
} {
    set bitness "32"
}

if { \
        $tcl_platform(platform) eq "windows" || \
       ($tcl_platform(platform) eq "unix" && [string match "CYGWIN*" $tcl_platform(os)]) \
} {
    set platform "win"
} elseif { \
        $tcl_platform(platform) eq "unix" && $tcl_platform(os) eq "Linux" \
} {
    set platform "lin"
} else {
    set platform "unk"
}

lappend auto_path [file join [file dirname [info script]] libs-${platform}-${bitness}] [file join [file dirname [info script]] libs-noarch]

unset platform bitness

package require http
package require tdom
package require tls    1.7
package require logger
package require ElectricCommander::Arguments
package require ElectricCommander::ResponseHandler
package require ElectricCommander::Logger

package provide ElectricCommander 0.0.1

namespace eval ::ElectricCommander {

    variable objectIdentifiers {
        credentialName
        groupName
        jobId
        jobStepId
        objectId
        procedureName
        projectName
        propertySheetId
        resourceName
        scheduleName
        stepName
        systemObjectName
        userName
        workspaceName
        resourcePoolName
        zoneName
        gatewayName
        artifactName
        repositoryName
        applicationName
        componentName
        processName
    }

}

::oo::class create ElectricCommander {

    variable ErrorMessage

    variable ProtocolVersion
    variable RequestCount
    variable Attachments

    variable DefaultUrl
    variable DefaultUser

    variable Url
    variable SecureUrl

    variable props
    variable log

    variable _response

    constructor { args } {

        set ProtocolVersion 2.2

        set log [::logger::init EC::[namespace tail [self]]]

        ${log}::debug "EC object created"

        set args [concat [list \
            -secure     [expr { [info exists ::env(COMMANDER_SECURE)]?"$::env(COMMANDER_SECURE)":"1" }] \
            -port       [expr { [info exists ::env(COMMANDER_PORT)]?"$::env(COMMANDER_PORT)":"8000" }] \
            -securePort [expr { [info exists ::env(COMMANDER_HTTPS_PORT)]?"$::env(COMMANDER_HTTPS_PORT)":"8443" }] \
            -server     [expr { [info exists ::env(COMMANDER_SERVER)]?"$::env(COMMANDER_SERVER)":"localhost" }] \
            -timeout 180 -retryTimeout -1 -format xml -checkArgs 1 \
            -dryRun 0 -abortOnError 1 -initialReconnectDelay 0.01 -backOffMultiplier 2 -maxReconnectDelay 30 -retry 0 \
            -sessionId "" -responseFile "" -user "" \
        ] $args]

        set _response [ElectricCommander::ResponseHandler new [self]]

        my configure {*}$args
        my configureUrls

    }

    destructor {
        $_response destroy
        ${log}::debug "EC object destroyed"
    }

    method response { args } {
        if { [llength $args] } {
            tailcall $_response {*}$args
        } {
            return $_response
        }
    }

    method configure { args } {

        array set validParameters {
            -server       server
            -port         port
            -secureport   securePort
            -secure       secure
            -timeout      timeout
            -sessionid    sessionId
            -format       format
            -checkargs    checkArgs
            -dryrun       dryRun
            -retrytimeout retryTimeout
            -responsefile responseFile
            -retry        retry
            -abortonerror abortOnError
            -initialreconnectdelay initialReconnectDelay
            -backoffmultiplier     backOffMultiplier
            -maxreconnectdelay     maxReconnectDelay
            -user                  User
        }

        if { [llength $args] } {

            if { [expr { [llength $args] % 2 }] != 0 } {
                return -code error "ElectricCommander::configure requires an even number of arguments"
            }

            foreach { k v } $args {

                set configName [string tolower $k]
                if { [string index $configName 0] ne "-" } {
                    set configName "-$configName"
                }

                if { ![info exists validParameters($configName)] } {
                    set errmsg "Invalid option \"$k\" to ElectricCommander::configure"
                    append errmsg "\nMust be one of: [join [array names validParameters] {, }]"
                    return -code error $errmsg
                }

                set props($validParameters($configName)) $v
                ${log}::debug "Setting parameter \"${configName}\": \"${v}\""
            }

        }

        return [array get props]
    }

    method configureUrls { } {

#        if { $Server eq "" && [info exists DefaultUrl] } {
#
#            ${log}::debug "Default url: $DefaultUrl"
#
#            if { ![regexp {^http(s?)://(.*):(.*)/(commanderRequest)?} -> mSecure mServer mPort] } {
#                ${log}::warn "Default url contains not expected value"
#            } {
#
#                set mSecure [expr { $mSecure eq "s" }]
#                set mSecurePort $mPort
#
#            }
#        }

#        if { $Server eq "" } {
#            set mServer "localhost"
#        }

#        if { ![info exists mSecure] } {
#            set mSecure     $Secure
#            set mPort       $Port
#            set mSecurePort $SecurePort
#        }

        set Url       "http://$props(server):$props(port)/commanderRequest"
        set SecureUrl "https://$props(server):$props(securePort)/commanderRequest"

    }

    method makeEnvelope { requests { mode "" } { ignoreErrors "" } } {

        if { $props(format) eq "json" } {
            return -code error "Not implemented."
        } {

            set xml  [dom createDocument "requests"]
            set root [$xml documentElement]
            $root setAttribute version $ProtocolVersion
            $root setAttribute timeout $props(timeout)

            if { $props(sessionId) ne "" } {
                $root setAttribute sessionId $props(sessionId)
            }

            if { $mode ne "" } {
                $root setAttribute mode $mode
            }

            if { $ignoreErrors ne "" } {
                $root setAttribute ignoreErrors $ignoreErrors
            }

            foreach request $requests {
                $root appendXML $request
            }

            set envelope {<?xml version="1.0" encoding="UTF-8"?>}
            append envelope "\n" [$root asXML]

            $xml delete

        }

        return $envelope

    }

    method makeRequest { command parameters { ignoreErrors "" } } {

        set requestId [incr RequestCount]

#        if { $CallSite } {
#
#            set original $requestId
#            append requestId "-[my callSite]"
#
#        }

        if { $props(format) eq "json" } {
            return -code error "Not implemented."
        } {

            set xml  [dom createDocument "request"]
            set root [$xml documentElement]
            $root setAttribute requestId $requestId

            if { $ignoreErrors ne "" } {
                $root setAttribute ignoreErrors $ignoreErrors
            }

            set commandNode [$xml createElement $command]
            $root appendChild $commandNode

            dict for { k v } $parameters {
                set node [$xml createElement $k]
                $node appendChild [$xml createTextNode $v]
                $commandNode appendChild $node
            }

            set request [$root asXML]

            $xml delete

        }

        return $request

    }

    method marshallCall { command suppliedRequired { suppliedOptional {} } } {

        set flags    [::ElectricCommander::Arguments::get_flags    $command]
        set required [::ElectricCommander::Arguments::get_required $command]
        set ignoreErrors ""
        array set parameters {}

        set numRequiredArgs [llength $required]

        if { [dict exists $flags minRequiredArgs] } {
            set minRequiredArgs [dict get $flags minRequiredArgs]
        } {
            set minRequiredArgs $numRequiredArgs
        }

        if { [llength $suppliedRequired] < $minRequiredArgs || [llength $suppliedRequired] > $numRequiredArgs } {
            set errmsg "Incorrect number of arguments to ElectricCommander::${command} "
            append errmsg [join $required " "]
            append errmsg " ?options?"
            return -code error $errmsg
        }

        foreach k $required v $suppliedRequired {
            set parameters($k) $v
        }

        if { [dict exists $suppliedOptional ignoreErrors] } {
            set ignoreErrors [dict get $suppliedOptional ignoreErrors]
            dict unset suppliedOptional ignoreErrors
        }

        dict for { k v } $suppliedOptional {
            set parameters($k) $v
        }

        if { $props(checkArgs) } {
            set validChoice [concat $required [::ElectricCommander::Arguments::get_optional $command]]
            foreach param [array names parameters] {
                if { [lsearch -exact $validChoice $param] == -1 } {
                    set errmsg "Invalid option \"$param\" to ElectricCommander::${command}"
                    append errmsg "\nMust be one of: [join $validChoice {, }]"
                    return -code error $errmsg
                }
            }
        }

        if { [dict get $flags locator] } {
            if { [info exists ::env(COMMANDER_JOBSTEPID)] && $::env(COMMANDER_JOBSTEPID) ne "" } {

                set isIdentifierSupplied 0
                foreach identifier $::ElectricCommander::objectIdentifiers {
                    if { [info exists parameters($identifier)] } {
                        set isIdentifierSupplied 1
                        break
                    }
                }

                if { [dict get $flags locator] == 2 || !$isIdentifierSupplied } {

                   if { ![info exists parameters(jobStepId)] || $parameters(jobStepId) eq "" } {

                       set parameters(jobStepId) $::env(COMMANDER_JOBSTEPID)

                   }

                }
            }
        }

        return [my makeRequest $command [array get parameters] $ignoreErrors]
    }

    method maskPassword { data } {
        regsub -all {<password>.+?</password>} $data {<password>[PROTECTED]</password>} data
        regsub -all {"password"\s*:\s*"[^""]*"} $data {"password":"[PROTECTED]"} data
        return $data
    }

    method truncateString { str { len 4096 } } {
        if { [string length $str] > $len } {
            set str [string range $str 0 [expr { $len - 1 }]]
            append str "\n... Truncated at $len characters ..."
        }
        return $str
    }

    method getUrl { } {
        return [expr { $props(secure)?"$SecureUrl":"$Url" }]
    }

    method httpPost { data } {

        ${log}::debug "Send data:\n[my truncateString [my maskPassword $data]]"

        if { $props(dryRun) } {
            return
        }

        set delay $props(initialReconnectDelay)
        set endTime [clock seconds]

        $_response destroy
        set _response [ElectricCommander::ResponseHandler new [self]]

        if { $props(retryTimeout) == -1 } {
            if { [info exists ::env(COMMANDER_JOBSTEPID)] && $::env(COMMANDER_JOBSTEPID) ne "" } {
                incr endTime [expr { 24 * 3600 }]
            } {
                incr endTime $props(timeout)
            }
        } {
            incr endTime $props(retryTimeout)
        }

        set firstAttempt 1
        while { 1 } {

            if { !$firstAttempt } {

                ${log}::debug "Retrying, delay=[expr { round(1000.0 * $delay) }]ms, retryTimeout=[expr { $endTime - [clock seconds] }]"

                unset -nocomplain [self namespace]::startRetry
                after [expr { round(1000.0 * $delay) }] [list set [self namespace]::startRetry 1]
                vwait [self namespace]::startRetry
                unset [self namespace]::startRetry

                set delay [expr { 1.0 * $props(backOffMultiplier) * $delay }]
                if { $delay > $props(maxReconnectDelay) } {
                    set delay $props(maxReconnectDelay)
                }

            } {
                set firstAttempt 0
            }

            set ErrorMessage ""
            set url [my getUrl]

            ${log}::debug "URL: $url"

            set cmd [list ::http::geturl $url -timeout [expr { 1000 * $props(timeout) }] -method POST]
            set headers [list]

            if { [info exists Attachments] } {
                return -code error "Not implemented."
            } {
                if { $props(format) eq "json" } {
                    return -code error "Not implemented."
                } {
                    lappend cmd -type "text/xml; charset=utf-8"
                    lappend cmd -query $data
                }
            }

            lappend cmd -headers $headers

            if { $props(responseFile) ne "" } {
                set fid [open $props(responseFile) w]
                lappend cmd -channel $fid
                ${log}::debug "Saving response to file: $props(responseFile)"
            }

            if { [info exists ::http::urlTypes(https)] } {
                set savedSslHandler $::http::urlTypes(https)
            }
            ::http::register https 443 [list ::tls::socket -ssl3 false -ssl2 false -tls1 true]

            set start [clock seconds]

            ${log}::debug "Starting request with timeout = $props(timeout) secs"
            set requestStatus [catch $cmd token]

            if { [info exists savedSslHandler] } {
                set ::http::urlTypes(https) $savedSslHandler
            } {
                unset ::http::urlTypes(https)
            }

            set elsaped [expr { [clock seconds] - $start }]

            if { $props(responseFile) ne "" } {
                close $fid
            }

            unset -nocomplain errmsg

            # Possible codes
            # 200 - OK
            # 204 - No content
            #   The server did not send a response document (or it was
            #   incomplete), but the operation was successful.
            # 401 - Unauthorized

            if { $requestStatus } {

                set errmsg "Request failed in ${elsaped} secs. Message: ${token}"

            } elseif { [::http::status $token] eq "timeout" } {

                set errmsg "Request failed with timeout, elsaped ${elsaped} secs."

            } elseif { [dict exists [::http::meta $token] "X-Died"] } {

                # The message was truncated mid-stream, typically because the
                # server shut down while sending the response.
                set errmsg "Incomplete response from server in ${elsaped} secs: [dict get [::http::meta $token] X-Died]"

            } elseif { [::http::ncode $token] ni "200 204 401" } {

                set errmsg "Request completed with error code in ${elsaped} secs: [::http::code $token]"
                if { [::http::size $token] } {
                    ${log}::debug "Server data:\n[::http::data $token]"
                }

            } elseif { ![::http::size $token] && [::http::ncode $token] ne "204" } {

                set errmsg "Request returns empty response in ${elsaped} secs"

            } {
                ${log}::debug "Request completed in ${elsaped} secs. [::http::code $token]"

#                array set meta [::http::meta $token]

#                ${log}::debug "REQ|error:  [::http::error  $token]"
#                ${log}::debug "REQ|status: [::http::status $token]"
#                ${log}::debug "REQ|code:   [::http::code   $token]"
#                ${log}::debug "REQ|ncode:  [::http::ncode  $token]"
#                ${log}::debug "REQ|size:   [::http::size   $token]"
#                foreach k [array names meta] {
#                  ${log}::debug "REQ|header|$k : $meta($k)"
#                }
#                ${log}::debug "REQ|data:\n[::http::data $token]"

#                unset meta

                 set responseSize [::http::size  $token]
                 set responseData [::http::data  $token]
                 set responseCode [::http::ncode $token]
                 set responseMeta [::http::meta  $token]

            }

            ::http::cleanup $token

            if { [info exists errmsg] } {
                ${log}::error $errmsg

                if { $Retry } {
                    if { [clock seconds] < $endTime } {
                        continue
                    }
                    ${log}::debug "Don't retrying, retryTimeout reached."
                }

                set ErrorMessage "error \[InternalFault\]: $errmsg"

            } elseif { $responseCode eq "204" } {

                # No content

            } else {

                if { $responseCode eq "401" } {
                    my unsetSessionId
                }

                ${log}::debug "Response from server:\n[my truncateString [my maskPassword $responseData]]"

                $_response destroy
                if { $props(format) eq "json" } {
                    return -code error "Not implemented."
                } {
                    set _response [ElectricCommander::ResponseHandler::XML new [self]]
                }

                if { ![my response parse $responseData] } {
                    set ErrorMessage "error: unrecognizable response from server:\n$responseData"
                    ${log}::error $ErrorMessage
                    break
                }

                set sep ""
                foreach error [my response findErrors] {
                    if { [dict get $error code] eq "ServerNotReady" } {
                        ${log}::warn "Server not ready, retrying"
                        if { [clock seconds] < $endTime } {
                            continue
                        }
                        ${log}::debug "Don't retrying, retryTimeout reached."
                    }
                    append ErrorMessage $sep
                    append ErrorMessage [format \
                        {error [%s]: %s} [dict get $error code] [dict get $error message] \
                    ]
                    if { [dict get $error details] ne "" } {
                        append ErrorMessage "\nDetails:\n[dict get $error details]"
                    }
                    set sep "\n"
                }

                foreach { k v } $responseMeta {

                    if { [string tolower $k] ne "set-cookie" } continue

                    set v [split [lindex [split $v {;}] 0] =]

                    set k [string trimright [lindex $v 0]]

                    if { $k ne "sessionId" } continue

                    set v [string trimleft [lindex $v 1]]

                    if { [string index $v 0] eq "\"" && [string index $v end] eq "\"" } {
                        set v [string range $v 1 end-1]
                    }

                    if { $props(sessionId) ne $v } {
                        my setSessionId $v
                    }

                }

            }

            break

        }

        if { $ErrorMessage ne "" } {
            my checkAbort
        }

        return [my response]

    }

    method checkAbort { } {

        if { $ErrorMessage eq "" || !$props(abortOnError) } {
            return
        }

        puts stderr $ErrorMessage
        exit 1

    }

    method sendRequests { requests { mode "" } { ignoreErrors "" } } {

        while { true } {
            set result [my httpPost [my makeEnvelope $requests $mode $ignoreErrors]]
            break
        }

        return $result
    }

    method setSessionId { sessionId } {
        if { $sessionId ne "" && $sessionId ne $props(sessionId) } {
            my configure -sessionId $sessionId
        }
    }

    method unsetSessionId { } {
        my configure -sessionId ""
    }

}

::oo::define ElectricCommander { method login { userName password } {

    set secure       $props(secure)
    set abortOnError $props(abortOnError)

    my configure -user $userName
    my configure -abortOnError 0 -secure 1

    set endTime [expr { [clock seconds] + $props(timeout) }]

    while 1 {

        set obj [my login_proc $userName $password]

        if { [clock seconds] < $endTime } {

            if { [$obj type] eq "EMPTY" } {
                ${log}::warn "Server response was empty, retrying"
                update idle
                continue
            }

            set notReady 0
            foreach error [$obj findErrors] {
                if { [dict get $error code] eq "ServerNotReady" } {
                    ${log}::warn "Server not ready, retrying"
                    set notReady 1
                    break
                }
            }
            if { $notReady } {
                update idle
                continue
            }

        }

        break

    }

    my configure -abortOnError $abortOnError -secure $secure
    my checkAbort

    if { [$obj type] eq "EMPTY" || [llength [$obj findErrors]] } {

        set ErrorMessage "login message failed\n$ErrorMessage"
        return $obj

    }

    my setSessionId [$obj findvalue "//sessionId"]

    return $obj
}}


::oo::define ElectricCommander { method getServerStatus { args } {

    set abortOnError $props(abortOnError)
    set retry        $props(retry)
    my configure -retry 1 -abortOnError 0

    set endTime [expr { [clock seconds] + $props(timeout) }]

    while 1 {

        set obj [my getServerStatus_proc {*}$args]

        if { [clock seconds] < $endTime && [$obj type] eq "EMPTY" } {
            ${log}::warn "Server response was empty, retrying"
            update idle
            continue
        }

        break

    }

    my configure -retry $retry -abortOnError $abortOnError
    my checkAbort

    return $obj

}}

::oo::define ElectricCommander { method getDatabaseConfiguration { args } {

    set secure $props(secure)
    my configure -secure 1

    set obj [my getDatabaseConfiguration_proc {*}$args]

    my configure -secure $secure

    return $obj

}}

::oo::define ElectricCommander { method setDatabaseConfiguration { args } {

    set secure $props(secure)
    my configure -secure 1

    set obj [my setDatabaseConfiguration_proc {*}$args]

    my configure -secure $secure

    return $obj

}}

::oo::define ElectricCommander { method createUser { args } {

    set secure $props(secure)
    my configure -secure 1

    set obj [my createUser_proc {*}$args]

    my configure -secure $secure

    return $obj

}}

::oo::define ElectricCommander { method modifyUser { args } {

    set secure $props(secure)
    my configure -secure 1

    set obj [my modifyUser_proc {*}$args]

    my configure -secure $secure

    return $obj

}}

# ------------------------------------------------------------------------
# Miscellaneous wrapped operations
# ------------------------------------------------------------------------

::oo::define ElectricCommander { method clone { args } {
    return [my clone_proc {*}$args]
}}

::oo::define ElectricCommander { method createJob { args } {
    return [my createJob_proc {*}$args]
}}

::oo::define ElectricCommander { method getUsers { args } {
    return [my getUsers_proc {*}$args]
}}

::oo::define ElectricCommander { method getGroups { args } {
    return [my getGroups_proc {*}$args]
}}

::oo::define ElectricCommander { method modifyJob { args } {
    return [my modifyJob_proc {*}$args]
}}

::oo::define ElectricCommander { method getReleases { args } {
    return [my getReleases_proc {*}$args]
}}

# ------------------------------------------------------------------------
# API methods
# ------------------------------------------------------------------------

foreach command [::ElectricCommander::Arguments::all_commands] {

    set method $command
    set flags  [::ElectricCommander::Arguments::get_flags $command]

    if { [dict get $flags wrap] } {
        append method "_proc"
    }

    ::oo::define ElectricCommander [list method $method args {

        set command [self method]

        if { [string range $command end-4 end] eq "_proc" } {
            set command [string range $command 0 end-5]
        }

        ${log}::debug "Called API \"$command\"; args: $args"

        if { [catch { set request [my marshallCall $command $args] } errmsg] } {
            return -code error $errmsg
        }

        return [my sendRequests [list $request]]

    }]

    unset flags method
}

unset command