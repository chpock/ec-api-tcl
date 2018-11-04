
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
                    dict set error [$child nodeName] [$child text]
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

    method find { xpath } {

        return [$root selectNodes $xpath]

    }

    method findHash { xpath } {

        set values [dict create]

        foreach node [$root selectNodes "${xpath}/*"] {

            dict set values [$node nodeName] [$node text]

        }

        return $values

    }

    method findvalue { xpath } {

        set node [lindex [$root selectNodes $xpath] 0]

        if { $node eq "" } {
            return ""
        }

        if { ![$node hasChildNodes] } {
            return ""
        }

        return [$node text]

    }

    method dump { } {
        return [$root asXML]
    }

    destructor {
        $xml delete
    }

}