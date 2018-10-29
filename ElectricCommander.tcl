
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

lappend auto_path \
    [file join [file dirname [info script]] libs-${platform}-${bitness}] \
    [file join [file dirname [info script]] libs-noarch]

unset platform bitness

package require http
package require tdom
package require tls    1.7
package require ElectricCommander::Arguments
package require ElectricCommander::ResponseHandler
package require ElectricCommander::Logger
package require ElectricCommander::Util
package require ElectricCommander::Dump

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

    variable ServerInfo

    variable props
    variable log

    constructor { args } {

        set ProtocolVersion 2.2

        if { [dict exists $args -ignoreEnvironment] } {
            set props(ignoreEnvironment) [dict get $args -ignoreEnvironment]
        } {
            set props(ignoreEnvironment) 0
        }

        if { [dict exists $args -debug] } {
            set log [::ElectricCommander::Logger::init EC::[namespace tail [self]] [dict get $args -debug]]
        } {
            set log [::ElectricCommander::Logger::init EC::[namespace tail [self]]]
        }

        ${log}::debug "EC object created"

        set args [concat [list \
            -secure                [my getEnv "COMMANDER_SECURE" 1]           \
            -port                  [my getEnv "COMMANDER_PORT" 8000]          \
            -securePort            [my getEnv "COMMANDER_HTTPS_PORT" 8443]    \
            -server                [my getEnv "COMMANDER_SERVER" "localhost"] \
            -timeout               180  \
            -format                xml  \
            -checkArgs             1    \
            -dryRun                0    \
            -abortOnError          1    \
            -initialReconnectDelay 0.01 \
            -backOffMultiplier     2    \
            -maxReconnectDelay     30   \
            -retry                 0    \
            -responseFile          ""   \
            -user                  ""   \
            -ignoreEnvironment     0    \
            -debug                 0    \
        ] $args]

        my configure {*}$args

        if { ![info exists props(retryTimeout)] } {

            if { [my getEnv "COMMANDER_JOBSTEPID"] ne "" } {
                set props(retryTimeout) [expr { 24 * 3600 }]
                ${log}::debug "Inside job step, setting parameter \"retryTimeout\": $props(retryTimeout)"
            } {
                set props(retryTimeout) $props(timeout)
                ${log}::debug "Standalone run, setting parameter \"retryTimeout\": $props(retryTimeout)"
            }

        }

        my configureUrls

    }

    destructor {
        ${log}::debug "EC object destroyed"
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
            -ignoreenvironment     ignoreEnvironment
            -debug                 debug
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

    method initSession { } {

        if { [info exists props(sessionId)] } {
            ${log}::debug "initSession - The session ID already defined"
            return
        }

        if { [my getEnv "COMMANDER_SESSIONID"] ne "" } {

            ${log}::debug "initSession - Found session in COMMANDER_SESSIONID"
            set props(sessionId) [my getEnv "COMMANDER_SESSIONID"]

        } elseif { [my getEnv "HTTP_COOKIE"] ne "" && [my getEnv "COMMANDER_HOME"] ne "" } {

            ${log}::debug "initSession - Loading session from CGI context"

            # TODO
            error "initSession - Loading session from CGI context is not implemented"

        } else {

            ${log}::debug "initSession - Loading session from session file"
            my findSession

        }

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

    # TODO
    method configureUser { } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method readSessionFile { } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method findSession { } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method loadSessionFile { } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method saveSessionFile { } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method httpPost { data } {

        ${log}::debug "Send data:\n[::ElectricCommander::Util::truncateString [::ElectricCommander::Util::maskSensitiveData $data]]"

        if { $props(dryRun) } {
            return
        }

        set delay $props(initialReconnectDelay)
        set endTime [clock seconds]
        incr endTime $props(retryTimeout)

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

            set cmd [list ::http::geturl $url -keepalive true -timeout [expr { 1000 * $props(timeout) }] -method POST]
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

            set start [clock milliseconds]

            ${log}::debug "Starting request with timeout = $props(timeout) secs"
            set requestStatus [catch $cmd token]

            if { [info exists savedSslHandler] } {
                set ::http::urlTypes(https) $savedSslHandler
            } {
                unset ::http::urlTypes(https)
            }

            set elsaped [format "%.3f" [expr { 0.001 * ([clock milliseconds] - $start) }]]

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

                if { $props(retry) } {
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

                ${log}::debug "Response from server:\n[::ElectricCommander::Util::truncateString [::ElectricCommander::Util::maskSensitiveData $responseData]]"

                if { $props(format) eq "json" } {
                    return -code error "Not implemented."
                } {
                    set response [ElectricCommander::ResponseHandler::XML new [self]]
                }

                if { ![$response parse $responseData] } {
                    set ErrorMessage "error: unrecognizable response from server:\n$responseData"
                    ${log}::error $ErrorMessage
                    break
                }

                set sep ""
                foreach error [$response findErrors] {
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

                    my setSessionId $v

                }

            }

            break

        }

        if { $ErrorMessage ne "" } {
            my checkAbort
        }

        return $response

    }

    method sendRequests { requests { mode "" } { ignoreErrors "" } } {

        while { true } {
            set result [my httpPost [my makeEnvelope $requests $mode $ignoreErrors]]
            break
        }

        return $result
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

    method makeEnvelope { requests { mode "" } { ignoreErrors "" } } {

        if { $props(format) eq "json" } {
            return -code error "Not implemented."
        } {

            set xml  [dom createDocument "requests"]
            set root [$xml documentElement]
            $root setAttribute version $ProtocolVersion
            $root setAttribute timeout $props(timeout)

            if { [info exists props(sessionId)] } {
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

    # TODO
    method checkAllErrors { } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method marshallCall { command args } {

        set flags    [::ElectricCommander::Arguments::get_flags    $command]
        set required [::ElectricCommander::Arguments::get_required $command]
        set optional [::ElectricCommander::Arguments::get_optional $command]
        set ignoreErrors ""
        array set _args [my normalizeArguments {*}$args]

        set numRequiredArgs [llength $required]

        if { [dict exists $flags minRequiredArgs] } {
            set minRequiredArgs [dict get $flags minRequiredArgs]
        } {
            set minRequiredArgs $numRequiredArgs
        }

        set count 1
        foreach k $required {

            if { ![info exists _args($k)] } {

                set errmsg "Incorrect number of arguments to ElectricCommander::${command}"
                foreach k $required { append errmsg " -$k <value>" }
                if { [llength $optional] } {
                    append errmsg " ?"
                    foreach k $optional { append errmsg " -$k <value>" }
                    append errmsg " ?"
                }
                return -code error $errmsg

            }

            if { [incr count] >= $minRequiredArgs } {
                break
            }

        }

        if { [info exists _args(ignoreErrors)] } {
            set ignoreErrors $_args(ignoreErrors)
            unset _args(ignoreErrors)
        }

        if { $props(checkArgs) } {
            set validChoice [concat $required $optional]
            foreach param [array names _args] {
                if { [lsearch -exact $validChoice $param] == -1 } {
                    set errmsg "Invalid option \"$param\" to ElectricCommander::${command}"
                    append errmsg "\nMust be one of: [join $validChoice {, }]"
                    return -code error $errmsg
                }
            }
        }

        if { [dict get $flags locator] } {
            if { [my getEnv "COMMANDER_JOBSTEPID"] ne "" } {

                set isIdentifierSupplied 0
                foreach identifier $::ElectricCommander::objectIdentifiers {
                    if { [info exists _args($identifier)] } {
                        set isIdentifierSupplied 1
                        break
                    }
                }

                if { [dict get $flags locator] == 2 || !$isIdentifierSupplied } {

                   if { ![info exists _args(jobStepId)] || $_args(jobStepId) eq "" } {

                       set _args(jobStepId) [my getEnv "COMMANDER_JOBSTEPID"]

                   }

                }
            }
        }

        tailcall my makeRequest $command [array get _args] $ignoreErrors
    }

    method setTimeout { {timeoutVal "" } } {

        set oldTimeout $props(timeout)

        if { $timeoutVal ne "" } {
            my configure -timeout $timeoutVal -retryTimeout $timeoutVal
        }

        return $oldTimeout

    }

    method setFormat { formatVal } {

        if { $formatVal in {xml json} } {

            if { $formatVal eq "json" } {
                # TODO
                error "setFormat: 'json' format is not implemented"
            }

            my configure -format $formatVal

        } {
            error "setFormat: Unsupported format ${formatVal}. Must be one of 'xml' or 'json'"
        }

    }

    method setSessionId { sessionId } {

        if { $sessionId eq "" } {
            my unsetSessionId
        } elseif { ![info exists props(sessionId)] || $sessionId ne $props(sessionId) } {
            my configure -sessionId $sessionId
        }

    }

    method abortOnError { { abortOnErrorVal "" } } {

        if { $abortOnErrorVal ne "" } {
            my configure -abortOnError $abortOnErrorVal
        }

        return $props(abortOnError)

    }

    # TODO
    method setAbortDefault { abortDefaultVal } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method checkAbort { } {

        if { $ErrorMessage eq "" || !$props(abortOnError) } {
            return
        }

        puts stderr [string map [list "\n" "\n    "] $ErrorMessage]
        exit 1

    }

    method getError { } {

        set msg $ErrorMessage
        set ErrorMessage ""

        return $msg

    }

    method getCurrentUser { } {

        return $props(User)

    }

    method unsetSessionId { } {
        unset -nocomplain props(sessionId)
        ${log}::debug "Unset session ID"
    }

    method login { args } {

        set secure       $props(secure)
        set abortOnError $props(abortOnError)

        if { [dict exists $args userName] } {
            my configure -user [dict get $args userName]
        } elseif { [dict exists $args -userName] } {
            my configure -user [dict get $args -userName]
        }

        my configure -abortOnError 0 -secure 1

        set endTime [expr { [clock seconds] + $props(timeout) }]

        while 1 {

            set obj [my login_proc {*}$args]

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
    }

    method getServerStatus { args } {

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

    }

    method getDatabaseConfiguration { args } {

        set secure $props(secure)
        my configure -secure 1

        set obj [my getDatabaseConfiguration_proc {*}$args]

        my configure -secure $secure

        return $obj

    }

    method setDatabaseConfiguration { args } {

        set secure $props(secure)
        my configure -secure 1

        set obj [my setDatabaseConfiguration_proc {*}$args]

        my configure -secure $secure

        return $obj

    }


    method createUser { args } {

        set secure $props(secure)
        my configure -secure 1

        set obj [my createUser_proc {*}$args]

        my configure -secure $secure

        return $obj

    }


    method modifyUser { args } {

        set secure $props(secure)
        my configure -secure 1

        set obj [my modifyUser_proc {*}$args]

        my configure -secure $secure

        return $obj

    }

    method runProcedure { args } {
        tailcall my jobWaiter [self method] {*}$args
    }

    method runProcess { args } {
        tailcall my jobWaiter [self method] {*}$args
    }

    method runServiceProcess { args } {
        tailcall my jobWaiter [self method] {*}$args
    }

    method tearDown { args } {
        tailcall my jobWaiter [self method] {*}$args
    }

    method tearDownEnvironment { args } {
        tailcall my jobWaiter [self method] {*}$args
    }

    method tearDownResource { args } {
        tailcall my jobWaiter [self method] {*}$args
    }

    method tearDownResourcePool { args } {
        tailcall my jobWaiter [self method] {*}$args
    }

    method jobWaiter { args } {

        set command [lindex $args 0]
        set args    [my normalizeArguments {*}[lrange $args 1 end]]

        set result [my "${command}_proc" {*}$args]

        if { $ErrorMessage ne "" } {
            return $result
        }

        if { [dict exists $args pollInterval] } {

            set jobId [$result findvalue "//jobId"]

            tailcall my waitForJob $jobId [dict get $args timeout]

        }

        return $result

    }

    method includeFileHelper { command auxArgName originalArgName args } {

        # TODO: Read values from files

        set command "${command}_proc"

        tailcall my $command {*}$args

    }

    # TODO
    method loadArgFromFile { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method SnapshotHelper { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method getSnapshotEnvironments { args } {
        tailcall my SnapshotHelper [self method] {*}$args
    }

    method getSnapshots { args } {
        tailcall my SnapshotHelper [self method] {*}$args
    }

    method getSnapshot { args } {
        tailcall my SnapshotHelper [self method] {*}$args
    }

    method createSnapshot { args } {
        tailcall my SnapshotHelper [self method] {*}$args
    }

    method deleteSnapshot { args } {
        tailcall my SnapshotHelper [self method] {*}$args
    }

    method modifySnapshot { args } {
        tailcall my SnapshotHelper [self method] {*}$args
    }

    # TODO
    method PluginHelper { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method EnvironmentInventoryHelper { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method getEnvironmentInventory { args } {
        tailcall my EnvironmentInventoryHelper [self method] {*}$args
    }

    # TODO
    method EnvironmentInventoryItemHelper { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method createEnvironmentInventoryItem { args } {
        tailcall my EnvironmentInventoryItemHelper [self method] {*}$args
    }

    method modifyEnvironmentInventoryItem { args } {
        tailcall my EnvironmentInventoryItemHelper [self method] {*}$args
    }

    method getEnvironmentInventoryItem { args } {
        tailcall my EnvironmentInventoryItemHelper [self method] {*}$args
    }

    method deleteEnvironmentInventoryItem { args } {
        tailcall my EnvironmentInventoryItemHelper [self method] {*}$args
    }

    method createPlugin { args } {
        tailcall my PluginHelper [self method] {*}$args
    }

    # TODO
    method FormalParameterHelper { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method getFormalParameter { args } {
        tailcall my FormalParameterHelper [self method] {*}$args
    }

    method createFormalParameter { args } {
        tailcall my FormalParameterHelper [self method] {*}$args
    }

    method deleteFormalParameter { args } {
        tailcall my FormalParameterHelper [self method] {*}$args
    }

    method modifyFormalParameter { args } {
        tailcall my FormalParameterHelper [self method] {*}$args
    }

    # TODO
    method evalDsl { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method createApplicationFromDeploymentPackage { args } {
        tailcall my includeFileHelper [self method] dslFile dslString {*}$args
    }

    # TODO
    method createCatalogItem { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method modifyCatalogItem { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method createStep { args } {
        tailcall my includeFileHelper [self method] commandFile command {*}$args
    }

    method modifyStep { args } {
        tailcall my includeFileHelper [self method] commandFile command {*}$args
    }

    method createProperty { args } {
        tailcall my includeFileHelper [self method] valueFile value {*}$args
    }

    method modifyProperty { args } {
        tailcall my includeFileHelper [self method] valueFile value {*}$args
    }

    method setProperty { args } {
        tailcall my includeFileHelper [self method] valueFile value {*}$args
    }

    method createEmailNotifier { args } {
        tailcall my includeFileHelper [self method] formattingTemplateFile formattingTemplate {*}$args
    }

    method modifyEmailNotifier { args } {
        tailcall my includeFileHelper [self method] formattingTemplateFile formattingTemplate {*}$args
    }

    method expandString { args } {
        tailcall my includeFileHelper expandString valueFile value {*}$args
    }

    method getFullCredential { args } {

        if { [set jobStepId [my getEnv "COMMANDER_JOBSTEPID"]] eq "" } {
            set ErrorMessage "error: getFullCredential is only allowed when called from inside a running step"
            my checkAbort
            return
        }

        tailcall my getFullCredential_proc {*}$args -jobStepId $jobStepId

    }

    # TODO
    method getSender { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method putFile { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method clone { args } {
        tailcall my clone_proc {*}$args
    }

    method createJob { args } {
        tailcall my createJob_proc {*}$args
    }

    method getUsers { args } {
        tailcall my getUsers_proc {*}$args
    }

    method getGroups { args } {
        tailcall my getGroups_proc {*}$args
    }

    method modifyJob { args } {
        tailcall my modifyJob_proc {*}$args
    }

    method getReleases { args } {
        tailcall my getReleases_proc {*}$args
    }

    method getWaitingTasks { args } {

        set args [my normalizeArguments {*}$args]

        if { [dict exists $args deployerTaskName] } {
            dict set args taskName [dict get $args deployerTaskName]
        }

        tailcall my getWaitingTasks_proc {*}$args

    }

    method createDeployerConfiguration { args } {
        tailcall my createDeployerConfiguration_proc {*}$args
    }

    method modifyDeployerConfiguration { args } {
        tailcall my modifyDeployerConfiguration_proc {*}$args
    }

    method getDeployerConfiguration { args } {
        tailcall my getDeployerConfiguration_proc {*}$args
    }

    method removeDeployerConfiguration { args } {
        tailcall my removeDeployerConfiguration_proc {*}$args
    }

    method validateDeployer { args } {
        tailcall my validateDeployer_proc {*}$args
    }

    # TODO
    method putFiles { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method getFiles { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method throwError { { msg {} } } {

        append ErrorMessage $msg
        my checkAbort
        ${log}::debug "error: $ErrorMessage"

    }

    # TODO
    method import { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method doPromotePlugin { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method requireXml { op } {
        if { $props(format) ne "xml" } {
            error "$op is only supported with xml format"
        }
    }

    # TODO
    method promotePlugin { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method uninstallPlugin { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method installPlugin { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method attachFile { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method getServerInfo { args } {

        set result [my getServerInfo_proc {*}$args]

        if { $ErrorMessage ne "" } {
            return $result
        }

        if { [llength [$result findErrors]] } {

            set ErrorMessage "Unable to retrieve server connection information"
            return $result

        }

        set values [$result findHash "//serverInfo"]

        foreach k {putFileDestination jobEventsDestination} {
            regsub {^(.*?)://} [dict get $values $k] {/\1/} tmp
            dict set values $k $tmp
        }

        set ServerInfo $values

        return $result

    }

    # TODO
    method getStomp { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method releaseStomp { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method waitForFlowRuntime { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method waitForJob { jobId timeout { finalStatus {} } } {

        if { $finalStatus eq "" } {
            set finalStatus "completed"
        }

        if { ![regexp {^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$} $jobId] } {
            set jobId [ \
                [my expandString
                    -value {$[/myJob/jobId]} \
                    -jobId {b1788bd4-da31-11e8-9438-005056bb6bca}\
                ] findvalue "//value"]
        }

        set destination "/topic/events.job"
        set selector    "path = '/jobs/$jobId'"

        if { $finalStatus eq "completed" } {

            append selector " and status = 'completed'"

        } elseif { $finalStatus ne "running" } {
            set ErrorMessage "error: waitForJob called with a status other than 'running' or 'completed'"
            my checkAbort
            return
        }

        set result [my getJobStatus -jobId $jobId]

        #TODO EventListener

        return $result

    }

    # TODO
    method newBatch { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method getUrl { } {
        return [expr { $props(secure)?"$SecureUrl":"$Url" }]
    }

    # TODO
    method sendEmail { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method loadProperties { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method cleanupArtifactCache { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method cleanupRepository { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method publishArtifactVersion { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method updateArtifactVersion { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method retrieveArtifactVersions { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    # TODO
    method getManifest { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method initArtifactManagement { args } {
        error "[lindex [info level 0] 1] - is not implemented"
    }

    method getPropertyValue { args } {

        set result [my getProperty {*}$args]

        if { ![llength [$result findErrors]] } {
            return [$result findvalue "//value"]
        }

        return ""

    }

    method getEnv { var { default {} } } {

        if { !$props(ignoreEnvironment) } {
            return $default
        }

        if { ![info exists ::env($var)] } {
            return $default
        }

        return $::env($var)

    }

    method normalizeArguments { args } {

        set result [list]

        foreach { k v } $args {
            if { [string index $k 0] eq "-" } {
                set k [string range $k 1 end]
            }
            lappend result $k $v
        }

        return $result

    }

}

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

        ${log}::info "API call \"$command\"; args: $args"

        if { [catch { set request [my marshallCall $command {*}$args] } errmsg] } {
            return -code error $errmsg
        }

        tailcall my sendRequests [list $request]

    }]

    unset flags method command
}
