package provide ElectricCommander::Dump 0.0.1

namespace eval ::ElectricCommander::Dump {

    variable namedObjects {
        -project          createProject
        -procedure        createProcedure
        -propertySheet    createPropertySheet
        -property         createProperty
        -formalParameter  createFormalParameter
        -actualParameter  createActualParameter
        -step             createStep
        -tag              tagObject
    }

    proc quoteValue { val } {

        set tmp "\"$val\""

        if { [string is list $tmp] && [llength $tmp] == 1 && [lindex $tmp 0] eq $val } {
            return $tmp
        }

        return [list $val]

    }

    proc smartQuoteValue { val } {

       if {
           ([string is boolean -strict $val] ||
           [string is double -strict $val] ||
           [string is entier -strict $val] ||
           [string is integer -strict $val] ||
           [string is xdigit -strict $val]) &&
           [string trim $val] eq $val
       } {
           return $val
       }

       return [quoteValue $val]

    }

    proc pretty { dump { level 0 } } {

        variable namedObjects

        set result [list]
        set pad [string repeat {    } $level]

        set count [llength $dump]
        for { set i 0 } { $i < $count } { incr i } {

            set k [lindex $dump $i]

            if { [dict exists $namedObjects $k ] } {

                lappend result "${pad}[list $k] [quoteValue [lindex $dump [incr i]]] {"
                lappend result {*}[pretty [lindex $dump [incr i]] [expr { $level + 1 }]]
                lappend result "${pad}}"

            } {

                lappend result "${pad}[list $k] [smartQuoteValue [lindex $dump [incr i]]]"

            }

        }

        if { $level } {
            return $result
        } {
            return [join $result \n]
        }

    }

    proc script { dump { level 0 } } {

        variable namedObjects

        set result [list]
        set pad [string repeat {    } $level]

        foreach { obj objName objArgs } $dump {

            set childs [list]
            set args   [list]

            set count [llength $objArgs]
            for { set i 0 } { $i < $count } { incr i } {

                set k [lindex $objArgs $i]
                set v [lindex $objArgs [incr i]]

                if { [dict exists $namedObjects $k] } {
                    lappend childs $k $v [lindex $objArgs [incr i]]
                } {
                    lappend args $k $v
                }

            }

            set objCmd  [list [dict get $namedObjects $obj]]
            set objName [quoteValue $objName]

            if { ![llength $args] && [llength $childs] } {

                lappend result "$pad$objCmd $objName -- \{"

                foreach { k v a } $childs {
                    lappend result "" {*}[script [list $k $v $a] [expr { $level + 1 }]]
                }

                lappend result "" "$pad\}"

            } elseif { [llength $args] && [llength $childs] } {

                lappend result "$pad$objCmd $objName -args \{"
                foreach { k v } $args {
                    lappend result "  $pad[list $k] [smartQuoteValue $v]"
                }
                lappend result "${pad}\} -- \{"

                foreach { k v a } $childs {
                    lappend result "" {*}[script [list $k $v $a] [expr { $level + 1 }]]
                }

                lappend result "" "$pad\}"

            } elseif { [llength $args] && ![llength $childs] } {

                lappend result "$pad$objCmd $objName -args \{"
                foreach { k v } $args {
                    lappend result "  $pad[list $k] [smartQuoteValue $v]"
                }
                lappend result "${pad}\}"

            } else {

                lappend result "$pad$objCmd $objName"

            }

        }

        if { $level } {
            return $result
        } {
            return [join $result \n]
        }

    }

}

namespace eval ::ElectricCommander::Dump::Export {

    proc project { EC projectName } {

        set result [list]

        set obj [$EC getProject -projectName $projectName]

        # The default state of 'tracked' filed depends
        # on EF settings. So, we have to always include it to
        # the dump.
        lappend result {*}[_getFields $obj "/responses/response/project" {
            credentialName ""
            description    ""
            resourceName   ""
            tracked        ""
            workspaceName  ""
        }]

        set obj [$EC getTags -projectName $projectName]

        foreach node [$obj find "/responses/response/tag/tagName"] {

            set tagName [$node text]

            lappend result -tag $tagName {}

        }

        set obj [$EC getProcedures -projectName $projectName]

        foreach node [$obj find "/responses/response/procedure/procedureName"] {

            set procedureName [$node text]

            lappend result {*}[procedure $EC $projectName $procedureName]

        }

        lappend result {*}[propertySheet $EC -projectName $projectName]

        return [list -project $projectName $result]

    }

    proc procedure { EC projectName procedureName } {

        set result [list]

        set obj [$EC getProcedure -projectName $projectName -procedureName $procedureName]

        lappend result {*}[_getFields $obj "/responses/response/procedure" {
            credentialName  ""
            description     ""
            jobNameTemplate ""
            resourceName    ""
            timeLimit       ""
            timeLimitUnits  ""
            workspaceName   ""
        }]

        lappend result {*}[formalParameters $EC -projectName $projectName -procedureName $procedureName]

        set obj [$EC getSteps -projectName $projectName -procedureName $procedureName]

        foreach node [$obj find "/responses/response/step/stepName"] {

            set stepName [$node text]

            lappend result {*}[step $EC $projectName $procedureName $stepName]

        }

        lappend result {*}[propertySheet $EC -projectName $projectName -procedureName $procedureName]

        return [list -procedure $procedureName $result]

    }

    proc step { EC projectName procedureName stepName } {

        set result [list]

        set obj [$EC getStep -projectName $projectName -procedureName $procedureName -stepName $stepName]

        lappend result {*}[_getFields $obj "/responses/response/step" {
            alwaysRun         0
            broadcast         0
            command           ""
            comment           ""
            condition         ""
            credentialName    ""
            description       ""
            errorHandling     failProcedure
            exclusive         0
            exclusiveMode     none
            logFileName       ""
            parallel          0
            postLogFileName   ""
            postProcessor     ""
            precondition      ""
            releaseExclusive  0
            releaseMode       none
            resourceName      ""
            shell             ""
            subprocedure      ""
            subproject        ""
            timeLimit         ""
            timeLimitUnits    ""
            workingDirectory  ""
            workspaceName     ""
        }]

        lappend result {*}[actualParameters $EC -projectName $projectName -procedureName $procedureName -stepName $stepName]

        lappend result {*}[propertySheet $EC -projectName $projectName -procedureName $procedureName -stepName $stepName]

        return [list -step $stepName $result]

    }

    proc actualParameters { EC args } {

        set result [list]

        set obj [$EC getActualParameters {*}$args]

        set count [llength [$obj find "/responses/response/actualParameter"]]

        for { set i 1 } { $i <= $count } { incr i } {

            set name [$obj findvalue "/responses/response/actualParameter\[$i\]/actualParameterName"]

            lappend result -actualParameter $name [_getFields $obj "/responses/response/actualParameter\[$i\]" {
                value ""
            }]

        }

        return $result

    }

    proc formalParameters { EC args } {

        set result [list]

        set obj [$EC getFormalParameters {*}$args]

        set count [llength [$obj find "/responses/response/formalParameter"]]

        for { set i 1 } { $i <= $count } { incr i } {

            set name [$obj findvalue "/responses/response/formalParameter\[$i\]/formalParameterName"]

            lappend result -formalParameter $name [_getFields $obj "/responses/response/formalParameter\[$i\]" {
                defaultValue       ""
                description        ""
                expansionDeferred  0
                gateType           ""
                label              ""
                orderIndex         ""
                required           0
                type               ""
            }]

        }

        return $result

    }

    proc propertySheet { EC args } {

        set obj [$EC getProperties -expand 0 -recurse 1 {*}$args]

        set cmd [list apply [list {cmd obj xpath} {

            set result [list]

            set countSheets [llength [$obj find "$xpath/propertySheet"]]

            for { set i 1 } { $i <= $countSheets } { incr i } {

                set countProps [llength [$obj find "$xpath/propertySheet\[$i\]/property"]]

                for { set j 1 } { $j <= $countProps } { incr j } {

                    set name [$obj findvalue "$xpath/propertySheet\[$i\]/property\[$j\]/propertyName"]

                    if { [llength [$obj find "$xpath/propertySheet\[$i\]/property\[$j\]/propertySheet"]] } {

                        lappend result -propertySheet $name [{*}$cmd $cmd $obj "$xpath/propertySheet\[$i\]/property\[$j\]"]

                    } {

                        lappend result -property $name [_getFields $obj "$xpath/propertySheet\[$i\]/property\[$j\]" {
                            counter             0
                            credentialProtected 0
                            description         ""
                            expandable          1
                            value               ""
                        }]

                    }

                }

            }

            return $result

        } [namespace current]]]

        return [{*}$cmd $cmd $obj "/responses/response"]

    }

    proc _getFields { obj xpath fields } {

        set result [list]

        foreach { field } [lsort [dict keys $fields]] {

            set val [$obj findvalue "$xpath/$field"]

            if { $val ne "" && $val ne [dict get $fields $field] } {
                lappend result "-$field" $val
            }

        }

        return $result

    }

}