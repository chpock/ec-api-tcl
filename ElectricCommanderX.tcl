package require ElectricCommander
package require procarg

::procarg::regtype commonId -expression {[regexp -nocase {^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$} $val]}

package provide ElectricCommanderX 0.0.1

::oo::class create ElectricCommanderX {

    variable EC

    variable Methods

    constructor { args } {

        if { [llength $args] } {

            if { [expr { [llength $args] % 2 }] != 0 } {
                return -code error "ElectricCommanderX::constructor requires an even number of arguments"
            }

            set _args [list]

            foreach { k v } $args {
                if { $k eq "-default" } {
                    set setDefault $v
                } {
                    lappend _args $k $v
                }
            }

            set args $_args

        }

        set EC [ElectricCommander new {*}$args]

        if { [info exists setDefault] } {
            my configure -default $setDefault
        }

        set Methods [concat \
            [info object methods [self] -all] \
            [info object methods $EC -all] \
        ]

    }

    destructor {
        my configure -default 0
        $EC destroy
    }

    method unknown { method args } {
        tailcall $EC $method {*}$args
    }

    method configure { args } {

        if { [llength $args] } {

            if { [expr { [llength $args] % 2 }] != 0 } {
                return -code error "ElectricCommanderX::constructor requires an even number of arguments"
            }

            set _args [list]

            foreach { k v } $args {
                if { $k eq "-default" } {
                    set setDefault $v
                } {
                    lappend _args $k $v
                }
            }

            set args $_args

        }

        if { [info exists setDefault] } {

            if { $setDefault } {

                if { [info exists ::__ec_obj_unknown] } {
                    $::__ec_obj_unknown configure -default 0
                }

                if { ![llength [info commands ::__ec_orig_unknown]] && [llength [info commands ::unknown]] } {
                    rename ::unknown ::_ec_orig_unknown
                }

                set ::__ec_obj_unknown [self]

                proc ::unknown { cmd args } {

                    if { [$::__ec_obj_unknown methodExists $cmd] } {
                        uplevel 1 [list $::__ec_obj_unknown $cmd {*}$args]
                    } {
                        uplevel 1 [list ::__ec_orig_unknown $cmd {*}$args]
                    }

                }

            } {

                if { [info exists ::__ec_obj_unknown] && [self] eq $::__ec_obj_unknown } {

                    unset ::__ec_obj_unknown

                    rename ::unknown ""

                    if { [llength [info commands ::__ec_orig_unknown]] } {
                        rename ::_ec_orig_unknown ::unknown
                    }

                }

            }

        }

        tailcall $EC [self method] {*}$args

    }

    method login { userName password } {
        tailcall $EC login -userName $userName -password $password
    }

    method expandString { value {args {
        {-applicationName             string}
        {-applicationTierName         string}
        {-artifactName                string}
        {-artifactVersionName         string}
        {-componentName               string}
        {-configName                  string}
        {-credentialName              string}
        {-environmentName             string}
        {-environmentTemplateName     string}
        {-environmentTemplateTierName string}
        {-environmentTierName         string}
        {-expand                      boolean}
        {-extendedContextSearch       switch}
        {-flowName                    string}
        {-flowRuntimeName             string}
        {-flowRuntimeStateName        string}
        {-flowStateName               string}
        {-flowTransitionName          string}
        {-gatewayName                 string}
        {-groupName                   string}
        {-jobId                       commonId}
        {-jobStepId                   commonId}
        {-notifierName                string}
        {-objectId                    commonId}
        {-path                        string}
        {-pipelineName                string}
        {-pluginName                  string}
        {-procedureName               string}
        {-processName                 string}
        {-processStepName             string}
        {-projectName                 string}
        {-propertySheetId             commonId}
        {-releaseName                 string}
        {-repositoryName              string}
        {-resourceName                string}
        {-resourcePoolName            string}
        {-resourceTemplateName        string}
        {-scheduleName                string}
        {-snapshotName                string}
        {-stageName                   string}
        {-stateDefinitionName         string}
        {-stateName                   string}
        {-stepName                    string}
        {-systemObjectName            string}
        {-taskName                    string}
        {-transitionDefinitionName    string}
        {-transitionName              string}
        {-userName                    string}
        {-workflowDefinitionName      string}
        {-workflowName                string}
        {-workspaceName               string}
        {-zoneName                    string}
    }} } {

        set cmd [my _makeCommand -args [list -value $value]]

        return [[{*}$cmd] findvalue "/responses/response/value"]

    }

    method deleteProperty { propertyName {args {
        {-applicationName             string}
        {-applicationTierName         string}
        {-artifactName                string}
        {-artifactVersionName         string}
        {-componentName               string}
        {-configName                  string}
        {-credentialName              string}
        {-environmentName             string}
        {-environmentTemplateName     string}
        {-environmentTemplateTierName string}
        {-environmentTierName         string}
        {-extendedContextSearch       switch}
        {-flowName                    string}
        {-flowRuntimeName             string}
        {-flowRuntimeStateName        string}
        {-flowStateName               string}
        {-flowTransitionName          string}
        {-gatewayName                 string}
        {-groupName                   string}
        {-jobId                       commonId}
        {-jobStepId                   commonId}
        {-notifierName                string}
        {-objectId                    commonId}
        {-path                        string}
        {-pipelineName                string}
        {-pluginName                  string}
        {-procedureName               string}
        {-processName                 string}
        {-processStepName             string}
        {-projectName                 string}
        {-propertySheetId             commonId}
        {-releaseName                 string}
        {-repositoryName              string}
        {-resourceName                string}
        {-resourcePoolName            string}
        {-resourceTemplateName        string}
        {-scheduleName                string}
        {-snapshotName                string}
        {-stageName                   string}
        {-stateDefinitionName         string}
        {-stateName                   string}
        {-stepName                    string}
        {-systemObjectName            string}
        {-taskName                    string}
        {-transitionDefinitionName    string}
        {-transitionName              string}
        {-userName                    string}
        {-workflowDefinitionName      string}
        {-workflowName                string}
        {-workspaceName               string}
        {-zoneName                    string}
    }} } {

        {*}[my _makeCommand -args [list -propertyName $propertyName]]

        return $propertyName

    }

    method setProperty { propertyName value {args {
        {-applicationName             string}
        {-applicationTierName         string}
        {-artifactName                string}
        {-artifactVersionName         string}
        {-componentName               string}
        {-configName                  string}
        {-credentialName              string}
        {-description                 string}
        {-environmentName             string}
        {-environmentTemplateName     string}
        {-environmentTemplateTierName string}
        {-environmentTierName         string}
        {-expandable                  boolean}
        {-extendedContextSearch       switch}
        {-flowName                    string}
        {-flowRuntimeName             string}
        {-flowRuntimeStateName        string}
        {-flowStateName               string}
        {-flowTransitionName          string}
        {-gatewayName                 string}
        {-groupName                   string}
        {-jobId                       commonId}
        {-jobStepId                   commonId}
        {-notifierName                string}
        {-objectId                    commonId}
        {-path                        string}
        {-pipelineName                string}
        {-pluginName                  string}
        {-procedureName               string}
        {-processName                 string}
        {-processStepName             string}
        {-projectName                 string}
        {-propertySheetId             commonId}
        {-releaseName                 string}
        {-repositoryName              string}
        {-resourceName                string}
        {-resourcePoolName            string}
        {-resourceTemplateName        string}
        {-scheduleName                string}
        {-snapshotName                string}
        {-stageName                   string}
        {-stateDefinitionName         string}
        {-stateName                   string}
        {-stepName                    string}
        {-systemObjectName            string}
        {-taskName                    string}
        {-transitionDefinitionName    string}
        {-transitionName              string}
        {-userName                    string}
        {-workflowDefinitionName      string}
        {-workflowName                string}
        {-workspaceName               string}
        {-zoneName                    string}
    }} } {

        {*}[my _makeCommand -args [list -propertyName $propertyName -value $value]]

        return $value

    }

    method getPropertyValue { propertyName {args {
        {-applicationName             string}
        {-applicationTierName         string}
        {-artifactName                string}
        {-artifactVersionName         string}
        {-componentName               string}
        {-configName                  string}
        {-credentialName              string}
        {-environmentName             string}
        {-environmentTemplateName     string}
        {-environmentTemplateTierName string}
        {-environmentTierName         string}
        {-expand                      boolean}
        {-extendedContextSearch       boolean}
        {-flowName                    string}
        {-flowRuntimeName             string}
        {-flowRuntimeStateName        string}
        {-flowStateName               string}
        {-flowTransitionName          string}
        {-gatewayName                 string}
        {-groupName                   string}
        {-jobId                       commonId}
        {-jobStepId                   commonId}
        {-notifierName                string}
        {-objectId                    commonId}
        {-path                        string}
        {-pipelineName                string}
        {-pluginName                  string}
        {-procedureName               string}
        {-processName                 string}
        {-processStepName             string}
        {-projectName                 string}
        {-propertySheetId             commonId}
        {-releaseName                 string}
        {-repositoryName              string}
        {-resourceName                string}
        {-resourcePoolName            string}
        {-resourceTemplateName        string}
        {-scheduleName                string}
        {-snapshotName                string}
        {-stageName                   string}
        {-stateDefinitionName         string}
        {-stateName                   string}
        {-stepName                    string}
        {-systemObjectName            string}
        {-taskName                    string}
        {-transitionDefinitionName    string}
        {-transitionName              string}
        {-userName                    string}
        {-workflowDefinitionName      string}
        {-workflowName                string}
        {-workspaceName               string}
        {-zoneName                    string}
    }} } {

        return [{*}[my _makeCommand -args [list -propertyName $propertyName]]]

    }

    method getProperties { {args {
        {-applicationName             string}
        {-applicationTierName         string}
        {-artifactName                string}
        {-artifactVersionName         string}
        {-componentName               string}
        {-configName                  string}
        {-credentialName              string}
        {-environmentName             string}
        {-environmentTemplateName     string}
        {-environmentTemplateTierName string}
        {-environmentTierName         string}
        {-expand                      boolean}
        {-flowName                    string}
        {-flowRuntimeName             string}
        {-flowRuntimeStateName        string}
        {-flowStateName               string}
        {-flowTransitionName          string}
        {-gatewayName                 string}
        {-groupName                   string}
        {-jobId                       commonId}
        {-jobStepId                   commonId}
        {-notifierName                string}
        {-objectId                    commonId}
        {-path                        string}
        {-pipelineName                string}
        {-pluginName                  string}
        {-procedureName               string}
        {-processName                 string}
        {-processStepName             string}
        {-projectName                 string}
        {-propertySheetId             commonId}
        {-recurse                     switch}
        {-releaseName                 string}
        {-repositoryName              string}
        {-resourceName                string}
        {-resourcePoolName            string}
        {-resourceTemplateName        string}
        {-scheduleName                string}
        {-snapshotName                string}
        {-stageName                   string}
        {-stateDefinitionName         string}
        {-stateName                   string}
        {-stepName                    string}
        {-systemObjectName            string}
        {-taskName                    string}
        {-transitionDefinitionName    string}
        {-transitionName              string}
        {-userName                    string}
        {-workflowDefinitionName      string}
        {-workflowName                string}
        {-workspaceName               string}
        {-zoneName                    string}
    }} } {

        set cmdParse [list apply [list {cmdParse obj xpath} {

            set result [list]

            set countSheets [llength [$obj find "$xpath/propertySheet"]]

            for { set i 1 } { $i <= $countSheets } { incr i } {

                set countProps [llength [$obj find "$xpath/propertySheet\[$i\]/property"]]

                for { set j 1 } { $j <= $countProps } { incr j } {

                    set name [$obj findvalue "$xpath/propertySheet\[$i\]/property\[$j\]/propertyName"]

                    if { [llength [$obj find "$xpath/propertySheet\[$i\]/property\[$j\]/propertySheet"]] } {

                        lappend result propertySheet $name [{*}$cmdParse $cmdParse $obj "$xpath/propertySheet\[$i\]/property\[$j\]"]

                    } {

                        lappend result property $name [my _getFields $obj "$xpath/propertySheet\[$i\]/property\[$j\]" {
                            value
                            expandable
                            description
                        }]

                    }

                }

            }

            return $result

        } [namespace current]]]

        return [{*}$cmdParse $cmdParse [{*}[my _makeCommand]] "/responses/response"]

    }

    method createProject { projectName {args {
        {-credentialName string}
        {-description    string}
        {-resourceName   string}
        {-tracked        boolean}
        {-workspaceName  string}
    }} } {

        {*}[my _makeCommand -args [list -projectName $projectName]]

        return $projectName

    }

    method getProjectNames { } {

        set obj [$EC getProjects]

        set result [list]

        foreach node [$obj find "/responses/response/project/projectName"] {

            lappend result [$node text]

        }

        return $result

    }

    method generateDsl { path {args {
        {-withAcls switch}
        {-file     string}
    }} } {

        set cmd [my _makeCommand -args [list -path $path] -ignore {-file}]

        set result [[{*}$cmd] findvalue "/responses/response/value"]

        if { [info exists opts(-file)] } {

            file mkdir [file dirname $opts(-file)]
            set fd [open $opts(-file) w]
            fconfigure $fd -encoding utf-8 -translation lf
            puts -nonewline $fd $result
            close $fd

        }

        return $result

    }

    method deleteProject { projectName {args {
        {-foreground switch}
    }} } {

        {*}[my _makeCommand -args [list -projectName $projectName]]

        return $projectName

    }

    method export { {args {
        {-raw     switch}
        {-project string}
    }} } {

        if { [info exists opts(-project)] } {

            set data [::ElectricCommander::Dump::Export::project $EC $opts(-project)]

        } {
            error "export entity is not specified"
        }

        if { !$opts(-raw) } {
            set data [::ElectricCommander::Dump::pretty $data]
        }

        return $data

    }

    method _makeCommand { {args {
        {-args   list -default {}}
        {-method string}
        {-ignore list -default {}}
    }} } {

        if { ![info exists opts(-method)] } {
            set opts(-method) [uplevel 1 [list self method]]
        }

        return [concat \
            [list $EC $opts(-method)] \
            $opts(-args) \
            [uplevel 1 [list my _autoparams $opts(-ignore)]] \
         ]

    }

    method _autoparams { ignoreParams } {

        upvar 1 opts opts

        set cmd [list]

        set method [uplevel 1 [list self method]]

        dict for { param - } [::procarg::getregistration $method] {
            if { $param ni $ignoreParams && [info exists opts($param)] } {
                lappend cmd $param $opts($param)
            }
        }

        return $cmd

    }

    method _getFields { obj xpath fields } {

        set result [list]

        foreach field [lsort $fields] {

            set node [lindex [$obj find "$xpath/$field"] 0]

            if { $node ne "" && [$node hasChildNodes] } {
                dict set result $field [$node text]
            }

        }

        return $result

    }

    method methodExists { methodName } {
        return [expr { $methodName in $Methods }]
    }

}

