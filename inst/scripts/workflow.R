## Demo to update an AnVIL workflow configuration

## define avworkspace namespace and name
avworkspace_namespace("bioconductor-rpci-anvil")
avworkspace_name("Bioconductor-Workflow-DESeq2")

## retrieve configuration of the workflow
config <- avworkflow_configuration_get("bioconductor-rpci-anvil", "AnVILBulkRNASeq")
config

## grab inputs and outpus
## define a new input
inputs <- avworkflow_configuration_template_inputs(config)
outputs <- avworkflow_configuration_template_outputs(config)
 
inputs
outputs

inputs$attribute[4] <- "\"new_index_name\""

## update current configuration with new input information

new_config <- avworkflow_configuration_update(config, inputs)
new_config 

## import the new version of workflow

avworkflow_configuration_set(new_config)

## run the new workflow

entityName <- avtable("participant_set") |> 
    pull(participant_set_id) |> 
    head(1)
avworkflow_run(new_config, entityName)


## now we can stop the workflow if we don't want it to complete

avworkflow_stop()
