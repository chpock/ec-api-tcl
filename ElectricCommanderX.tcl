package require ElectricCommander
package require procarg

::procarg::regtype commonId -expression {[regexp -nocase {^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$} $val]}

package provide ElectricCommanderX 0.0.1

::oo::class create ElectricCommanderX {

    variable EC

    constructor { args } {

        set EC [ElectricCommander new {*}$args]

    }

    destructor {
        $EC destroy
    }

    method unknown { method args } {
        tailcall $EC $method {*}$args
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

        procarg::parse

        set cmd [list $EC [self method] -value $value]

        my _autoparams {
            -applicationName
            -applicationTierName
            -artifactName
            -artifactVersionName
            -componentName
            -configName
            -credentialName
            -environmentName
            -environmentTemplateName
            -environmentTemplateTierName
            -environmentTierName
            -expand
            -extendedContextSearch
            -flowName
            -flowRuntimeName
            -flowRuntimeStateName
            -flowStateName
            -flowTransitionName
            -gatewayName
            -groupName
            -jobId
            -jobStepId
            -notifierName
            -objectId
            -path
            -pipelineName
            -pluginName
            -procedureName
            -processName
            -processStepName
            -projectName
            -propertySheetId
            -releaseName
            -repositoryName
            -resourceName
            -resourcePoolName
            -resourceTemplateName
            -scheduleName
            -snapshotName
            -stageName
            -stateDefinitionName
            -stateName
            -stepName
            -systemObjectName
            -taskName
            -transitionDefinitionName
            -transitionName
            -userName
            -workflowDefinitionName
            -workflowName
            -workspaceName
            -zoneName
        }

        return [[{*}$cmd] findvalue "/responses/response/value"]

    }

    method getProperty { propertyName {args {
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

        procarg::parse

        set cmd [list $EC [self method] -propertyName $propertyName]

        my _autoparams {
            -applicationName
            -applicationTierName
            -artifactName
            -artifactVersionName
            -componentName
            -configName
            -credentialName
            -environmentName
            -environmentTemplateName
            -environmentTemplateTierName
            -environmentTierName
            -expand
            -extendedContextSearch
            -flowName
            -flowRuntimeName
            -flowRuntimeStateName
            -flowStateName
            -flowTransitionName
            -gatewayName
            -groupName
            -jobId
            -jobStepId
            -notifierName
            -objectId
            -path
            -pipelineName
            -pluginName
            -procedureName
            -processName
            -processStepName
            -projectName
            -propertySheetId
            -releaseName
            -repositoryName
            -resourceName
            -resourcePoolName
            -resourceTemplateName
            -scheduleName
            -snapshotName
            -stageName
            -stateDefinitionName
            -stateName
            -stepName
            -systemObjectName
            -taskName
            -transitionDefinitionName
            -transitionName
            -userName
            -workflowDefinitionName
            -workflowName
            -workspaceName
            -zoneName
        }

        return [[{*}$cmd] findvalue "/responses/response/value"]

    }

    method getProjects { } {

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

        procarg::parse

        set cmd [list $EC [self method] -path $path]

        my _autoparams {
            -withAcls
        }

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

        set cmd [list $EC [self method] -projectName $projectName]

        my _autoparams {
            -foreground
        }

        return [expr { ![llength [[{*}$cmd] findErrors]] }]

    }

    method export { {args {
        {-raw     switch}
        {-project string}
    }} } {

        procarg::parse

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

    method _autoparams { params } {

        upvar 1 opts opts
        upvar 1 cmd  cmd

        foreach param $params {
            if { [info exists opts($param)] } {
                lappend cmd $param $opts($param)
            }
        }

    }

}

