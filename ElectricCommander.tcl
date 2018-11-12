package require ElectricCommander::RawAPI
package require procarg

::procarg::regtype commonId -expression {[regexp -nocase {^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$} $val]}

package provide ElectricCommander 0.0.1

::oo::class create ElectricCommander {

    variable EC
    variable Methods
    variable Defaults

    constructor { {args {
        {-default               switch}
        {-secure                boolean}
        {-port                  integer}
        {-securePort            integer}
        {-server                string}
        {-timeout               integer}
        {-format                string}
        {-checkArgs             boolean}
        {-dryRun                boolean}
        {-abortOnError          string -restrict {1 true yes 0 false no return} -default "return"}
        {-initialReconnectDelay double}
        {-backOffMultiplier     integer}
        {-maxReconnectDelay     integer}
        {-retry                 boolean}
        {-responseFile          string}
        {-user                  string}
        {-ignoreEnvironment     boolean}
        {-debug                 string -restrict {1 true yes 0 false no emergency alert critical error warning notice info debug} -default 0}
    }} } {

        set isDefault $opts(-default)
        unset opts(-default)

        set EC [ElectricCommander::RawAPI new {*}[array get opts]]

        if { $isDefault } {
            my configure -default
        }

        set Methods [concat \
            [info object methods [self] -all] \
            [info object methods $EC -all] \
        ]

        array set Defaults [list]

    }

    destructor {
        my configure -unsetdefault
        $EC destroy
    }

    method unknown { method args } {
        tailcall $EC $method {*}$args
    }

    method configure { {args {
        {-default               switch}
        {-unsetdefault          switch}
        {-secure                boolean}
        {-port                  integer}
        {-securePort            integer}
        {-server                string}
        {-timeout               integer}
        {-format                string}
        {-checkArgs             boolean}
        {-dryRun                boolean}
        {-abortOnError          string -restrict {1 true yes 0 false no return} -default "return"}
        {-initialReconnectDelay double}
        {-backOffMultiplier     integer}
        {-maxReconnectDelay     integer}
        {-retry                 boolean}
        {-responseFile          string}
        {-user                  string}
        {-ignoreEnvironment     boolean}
        {-debug                 string -restrict {1 true yes 0 false no emergency alert critical error warning notice info debug} -default 0}
    }} } {

        set isDefault $opts(-default)
        set isUnsetDefault $opts(-unsetdefault)
        unset opts(-default)
        unset opts(-unsetdefault)

        if { $isDefault } {

            if { [info exists ::__ec_obj_unknown] } {
                $::__ec_obj_unknown configure -unsetdefault
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

        } elseif { $isUnsetDefault } {
             if { [info exists ::__ec_obj_unknown] && [self] eq $::__ec_obj_unknown } {
                unset ::__ec_obj_unknown
                rename ::unknown ""
                if { [llength [info commands ::__ec_orig_unknown]] } {
                    rename ::_ec_orig_unknown ::unknown
                }
             }
        }

        tailcall $EC [self method] {*}[array get opts]

    }

    method login { userName password } {

        return [[$EC login -userName $userName -password $password] \
            findHash "/responses/response"]

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
        {-overwrite      switch}
        {--              string}
        {-args           args}
    }} } {

        if { $opts(-overwrite) } {
            $EC deleteProject -projectName $projectName -foreground 1
        }

        set result [[{*}[my _makeCommand \
            -args [list -projectName $projectName] \
            -ignore {-overwrite}
        ]] findHash "/responses/response/project"]

        my _runInside -projectName $projectName

        return $result

    }

    method createProcedure { procedureName {args {
        {-projectName     string}
        {-credentialName  string}
        {-description     string}
        {-jobNameTemplate string}
        {-resourceName    string}
        {-timeLimit       string}
        {-timeLimitUnits  string -restrict {hours minutes seconds}}
        {-workspaceName   string}
        {--               string}
        {-args            args}
    }} } {

        set result [[{*}[my _makeCommand \
            -args [list -procedureName $procedureName] \
        ]] findHash "/responses/response/procedure"]

        my _runInside -procedureName $procedureName

        return $result

    }

    method createStep { stepName {args {
        {-projectName         string}
        {-procedureName       string}
        {-afterProcedureStep  string}
        {-alwaysRun           boolean}
        {-beforeProcedureStep string}
        {-broadcast           boolean}
        {-command             string}
        {-comment             string}
        {-commandFile         string}
        {-condition           string}
        {-credentialName      string}
        {-description         string}
        {-errorHandling       string -restrict {failProcedure abortProcedure abortProcedureNow abortJob abortJobNow ignore}}
        {-exclusive           boolean}
        {-exclusiveMode       string -restrict {None Job Step Call}}
        {-logFileName         string}
        {-parallel            boolean}
        {-postProcessor       string}
        {-precondition        string}
        {-releaseExclusive    boolean}
        {-releaseMode         string -restrict {none release releaseToJob}}
        {-resourceName        string}
        {-shell               string}
        {-subprocedure        string}
        {-subproject          string}
        {-timeLimit           string}
        {-timeLimitUnits      string -restrict {hours minutes seconds}}
        {-workingDirectory    string}
        {-workspaceName       string}
        {--                   string}
        {-args                args}
    }} } {

        set result [[{*}[my _makeCommand \
            -args [list -stepName $stepName] \
        ]] findHash "/responses/response/step"]

        my _runInside -stepName $stepName

        return $result

    }

    method createFormalParameter { formalParameterName {args {
        {-projectName            string}
        {-procedureName          string}
        {-applicationName        string}
        {-componentName          string}
        {-defaultValue           string}
        {-description            string}
        {-expansionDeferred      boolean}
        {-flowName               string}
        {-flowStateName          string}
        {-gateType               string -restrict {POST PRE}}
        {-label                  string}
        {-orderIndex             integer}
        {-pipelineName           string}
        {-processName            string}
        {-processStepName        string}
        {-releaseName            string}
        {-required               boolean}
        {-serviceName            string}
        {-stageName              string}
        {-stateDefinitionName    string}
        {-stateName              string}
        {-taskName               string}
        {-type                   string}
        {-workflowDefinitionName string}
        {-workflowName           string}
        {-args                   args}
    }} } {

        return [[{*}[my _makeCommand \
            -args [list -formalParameterName $formalParameterName] \
        ]] findHash "/responses/response/formalParameter"]

    }

    method tagObject { tag {args {
        {-applicationName         string}
        {-artifactName            string}
        {-artifactVersionName     string}
        {-componentName           string}
        {-entityId                string}
        {-entityType              commonId}
        {-environmentName         string}
        {-environmentTemplateName string}
        {-flowRuntimeId           commonId}
        {-gateType                string -restrict {POST PRE}}
        {-jobId                   commonId}
        {-pipelineName            string}
        {-procedureName           string}
        {-processName             string}
        {-processStepName         string}
        {-projectName             string}
        {-releaseName             string}
        {-resourceName            string}
        {-resourceTemplateName    string}
        {-serviceName             string}
        {-stageName               string}
        {-stepName                string}
        {-taskName                string}
        {-workflowDefinitionName  string}
        {-workflowId              commonId}
        {-args                    args}
    }} } {

        return [[{*}[my _makeCommand \
            -args [list -tag $tag] \
        ]] findHash "/responses/response/tag"]

    }

    method createActualParameter { actualParameterName {args {
        {-projectName              string}
        {-procedureName            string}
        {-stepName                 string}
        {-applicationName          string}
        {-componentName            string}
        {-flowName                 string}
        {-flowStateName            string}
        {-processName              string}
        {-processStepName          string}
        {-releaseName              string}
        {-scheduleName             string}
        {-serviceName              string}
        {-stateDefinitionName      string}
        {-transitionDefinitionName string}
        {-value                    string}
        {-workflowDefinitionName   string}
        {-args                     args}
    }} } {

        return [[{*}[my _makeCommand \
            -args [list -actualParameterName $actualParameterName] \
        ]] findHash "/responses/response/actualParameter"]

    }

    method createPropertySheet { propertySheetName {args {
        {-applicationName             string}
        {-applicationTierName         string}
        {-artifactName                string}
        {-artifactVersionName         string}
        {-componentName               string}
        {-configName                  string}
        {-counter                     boolean}
        {-credentialName              string}
        {-credentialProtected         boolean}
        {-description                 string}
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
        {--                           string}
        {-args                        args}
    }} } {

        if { [info exists Defaults(-propertySheetName)] } {
            set propertySheetName "[lindex $Defaults(-propertySheetName) end]/$propertySheetName"
        }

        set result [[{*}[my _makeCommand \
            -method createProperty \
            -args   [list -propertyName $propertySheetName -propertyType sheet] \
        ]] findHash "/responses/response/property"]

        my _runInside -propertySheetName $propertySheetName

        return $result

    }

    method createProperty { propertyName {args {
        {-applicationName             string}
        {-applicationTierName         string}
        {-artifactName                string}
        {-artifactVersionName         string}
        {-componentName               string}
        {-configName                  string}
        {-counter                     boolean}
        {-credentialName              string}
        {-credentialProtected         boolean}
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
        {-value                       string}
        {-workflowDefinitionName      string}
        {-workflowName                string}
        {-workspaceName               string}
        {-zoneName                    string}
        {-args                        args}
    }} } {

        if { [info exists Defaults(-propertySheetName)] } {
            set propertyName "[lindex $Defaults(-propertySheetName) end]/$propertyName"
        }

        return [[{*}[my _makeCommand \
            -args   [list -propertyName $propertyName -propertyType string] \
        ]] findHash "/responses/response/property"]

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
        {-background switch}
    }} } {

        set opts(-foreground) [expr { !$opts(-background) }]
        unset opts(-background)

        {*}[my _makeCommand -args [list -projectName $projectName]]

        return $projectName

    }

    method export { {args {
        {-result      string -restrict {raw rawpretty script} -default script}
        {-projectName string}
        {-file        string}
    }} } {

        if { [info exists opts(-projectName)] } {

            set data [::ElectricCommander::Dump::Export::project $EC $opts(-projectName)]

        } {
            error "export entity is not specified"
        }

        switch -exact -- $opts(-result) {
            rawpretty { set data [::ElectricCommander::Dump::pretty $data] }
            script    { set data [::ElectricCommander::Dump::script $data] }
        }

        if { [info exists opts(-file)] } {

            file mkdir [file dirname $opts(-file)]
            set fd [open $opts(-file) w]
            fconfigure $fd -encoding utf-8 -translation lf
            puts -nonewline $fd $data
            close $fd

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

    method _runInside { args } {

        upvar 1 opts opts

        if { ![info exists opts(--)] } {
            return
        }

        foreach { k v } $args {
            if { $k eq "-propertySheetName" } {
                lappend Defaults($k) $v
            } else {
                set Defaults($k) $v
            }
        }

        set code [catch [list uplevel 2 $opts(--)] result]

        foreach { k - } $args {
            if { $k eq "-propertySheetName" } {
                if { ![llength [set Defaults($k) [lrange $Defaults($k) 0 end-1]]] } {
                    unset Defaults($k)
                }
            } else {
                unset Defaults($k)
            }
        }

        switch -exact -- $code {
            0 { # TCL_OK
                return $result
            }
            1 { # TCL_ERROR
                return -code error -errorcode $::errorCode -errorinfo $::errorInfo $result
            }
            2 { # TCL_RETURN
                return -code return $result
            }
            3 { # TCL_BREAK
                return -code break
            }
            4 { # TCL_CONTINUE
                return -code continue
            }
            default { # unknown
                return -code $code $result
            }
        }

    }

    method _autoparams { ignoreParams } {

        upvar 1 opts opts

        set cmd [list]

        set method [uplevel 1 [list self method]]

        dict for { param - } [::procarg::getregistration $method] {
            if { $param ne "--" && $param ne "-args" && $param ni $ignoreParams } {
                if { [info exists opts($param)] } {
                    lappend cmd $param $opts($param)
                } elseif { [info exists Defaults($param)] } {
                    lappend cmd $param $Defaults($param)
                }
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

