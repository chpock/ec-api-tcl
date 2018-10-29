package provide ElectricCommander::Dump 0.0.1

namespace eval ::ElectricCommander::Dump {

    variable namedObjects {
        -project
        -procedure
        -propertySheet
        -property
        -formalParameter
        -actualParameter
        -step
    }

    proc pretty { dump { level 0 } } {

        variable namedObjects

        set result [list]
        set pad [string repeat {    } $level]

        set count [llength $dump]
        for { set i 0 } { $i < $count } { incr i } {

            set k [lindex $dump $i]

            if { $k in $namedObjects } {

                lappend result "${pad}[list $k] [list [lindex $dump [incr i]]] {"
                lappend result {*}[pretty [lindex $dump [incr i]] [expr { $level + 1 }]]
                lappend result "${pad}}"

            } {

                lappend result "${pad}[list $k] [list [lindex $dump [incr i]]]"

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

        lappend result {*}[_getFields $obj "//project" {
            description
            workspaceName
            resourceName
        }]

        set obj [$EC getProcedures -projectName $projectName]

        foreach node [$obj find "//procedure/procedureName"] {

            set procedureName [$node text]

            lappend result {*}[procedure $EC $projectName $procedureName]

        }

        #lappend result {*}[propertySheet $EC -projectName $projectName]

        return [list -project $projectName $result]

    }

    proc procedure { EC projectName procedureName } {

        set result [list]

        set obj [$EC getProcedure -projectName $projectName -procedureName $procedureName]

        lappend result {*}[_getFields $obj "//procedure" {
            description
            jobNameTemplate
            resourceName
            timeLimitUnits
            workspaceName
        }]

        lappend result {*}[formalParameters $EC -projectName $projectName -procedureName $procedureName]

        set obj [$EC getSteps -projectName $projectName -procedureName $procedureName]

        foreach node [$obj find "//step/stepName"] {

            set stepName [$node text]

            lappend result {*}[step $EC $projectName $procedureName $stepName]

        }

        lappend result {*}[propertySheet $EC -projectName $projectName -procedureName $procedureName]

        return [list -procedure $procedureName $result]

    }

    proc step { EC projectName procedureName stepName } {

        set result [list]

        set obj [$EC getStep -projectName $projectName -procedureName $procedureName -stepName $stepName]

        lappend result {*}[_getFields $obj "//step" {
            alwaysRun
            broadcast
            command
            condition
            description
            errorHandling
            exclusive
            exclusiveMode
            logFileName
            parallel
            postLogFileName
            postProcessor
            releaseExclusive
            releaseMode
            resourceName
            shell
            timeLimit
            timeLimitUnits
            workingDirectory
            workspaceName
        }]

        lappend result {*}[actualParameters $EC -projectName $projectName -procedureName $procedureName -stepName $stepName]

        lappend result {*}[propertySheet $EC -projectName $projectName -procedureName $procedureName -stepName $stepName]

        return [list -step $stepName $result]

    }

    proc actualParameters { EC args } {

        set result [list]

        set obj [$EC getActualParameters {*}$args]

        set count [llength [$obj find "//actualParameter"]]

        for { set i 1 } { $i <= $count } { incr i } {

            set name [$obj findvalue "//actualParameter\[$i\]/actualParameterName"]

            set tmp [list]

            lappend tmp {*}[_getFields $obj "//actualParameter\[$i\]" {
                value
            }]

            lappend result -actualParameter $name $tmp

        }

        return $result

    }

    proc formalParameters { EC args } {

        set result [list]

        set obj [$EC getFormalParameters {*}$args]

        set count [llength [$obj find "//formalParameter"]]

        for { set i 1 } { $i <= $count } { incr i } {

            set name [$obj findvalue "//formalParameter\[$i\]/formalParameterName"]

            set tmp [list]

            lappend tmp {*}[_getFields $obj "//formalParameter\[$i\]" {
                description
                expansionDeferred
                label
                orderIndex
                required
                type
            }]

            lappend result -formalParameter $name $tmp

        }

        return $result

    }

    proc propertySheet { EC args } {

        set obj [$EC getProperties -recurse 1 {*}$args]

        set cmd [list apply [list {cmd obj xpath} {

            set result [list]

            #puts "XPATH: $xpath"

            set countSheets [llength [$obj find "$xpath/propertySheet"]]

            for { set i 1 } { $i <= $countSheets } { incr i } {

                set countProps [llength [$obj find "$xpath/propertySheet\[$i\]/property"]]

                for { set j 1 } { $j <= $countProps } { incr j } {

                    set name [$obj findvalue "$xpath/propertySheet\[$i\]/property\[$j\]/propertyName"]

                    #puts "$name $i $j [llength [$obj find "$xpath/propertySheet\[$i\]/property\[$j\]/propertySheet"]]"

                    if { [llength [$obj find "$xpath/propertySheet\[$i\]/property\[$j\]/propertySheet"]] } {

                        lappend result -propertySheet $name [{*}$cmd $cmd $obj "$xpath/propertySheet\[$i\]/property\[$j\]"]

                    } {

                        set tmp [list]

                        lappend tmp {*}[_getFields $obj "$xpath/propertySheet\[$i\]/property\[$j\]" {
                            value
                            expandable
                            description
                        }]

                        lappend result -property $name $tmp

                    }

                    #puts "TMP: $tmp"

                }

            }

            #puts "RET: $result"

            return $result

        } [namespace current]]]

#        set result [list]

        set result [{*}$cmd $cmd $obj "/responses/response"]

#        foreach { k v } [{*}$cmd $cmd $obj "/responses/response"] {
#            lappend result -property [list $k {*}$v]
#        }

        #puts "X: $result"

        return $result

    }

    proc _getFields { obj xpath fields } {

        set result [list]

        foreach field [lsort $fields] {

            set val [$obj findvalue "$xpath/$field"]

            if { $val ne "" } {
                lappend result "-$field" $val
            }

        }

        return $result

    }

}