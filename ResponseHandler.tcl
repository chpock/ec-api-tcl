
package provide ElectricCommander::ResponseHandler 0.0.1

::oo::class create ElectricCommander::ResponseHandler {

    variable EC
    variable TYPE

    constructor { ec } {
        set EC $ec
        set TYPE "EMPTY"
    }

    method parse { data } {
    }

    method findErrors { } {
    }

    method type { } {
        my variable TYPE

        return $TYPE
    }

}

::oo::class create ElectricCommander::ResponseHandler::JSON {
    superclass ElectricCommander::ResponseHandler
}

::oo::class create ElectricCommander::ResponseHandler::XML {
    superclass ElectricCommander::ResponseHandler

    variable root
    variable xml

    method parse { data } {
        my variable TYPE

        if { [catch { set xml [dom parse $data] } errmsg] } {
            set xml  [dom createDocument "empty"]
            set root [$xml documentElement]
            set TYPE "EMPTY"
            return 0
        }
        set root [$xml documentElement]
        set TYPE "XML"
        return 1
    }

    method findErrors { } {

        set errors [list]

        foreach node [$root selectNodes /responses/error] {

            set error [dict create]

            foreach child [$node childNodes] {
                if { [$child hasChildNodes] } {
                    dict set error [$child nodeName] [[$child firstChild] nodeValue]
                } {
                    dict set error [$child nodeName] ""
                }

            }

            lappend errors $error
        }

        return $errors
    }

    method findResponse { requestId } {
    }

    method findHash { xpath } {
    }

    method findvalue { xpath } {

        set node [lindex [$root selectNodes $xpath] 0]

        if { $node eq "" } {
            return ""
        }

        if { ![$node hasChildNodes] } {
            return ""
        }

        return [[$node firstChild] nodeValue]

    }

    destructor {
        $xml delete
    }
}