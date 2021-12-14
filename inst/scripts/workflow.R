## Demo to update an AnVIL workflow configuration

## define avworkspace namespace and name
avworkspace_namespace("bioconductor-rpci-anvil")
avworkspace_name("Bioconductor-Workflow-DESeq2")

## retrieve configuration of the workflow
config <- avworkflow_configuration("bioconductor-rpci-anvil", "AnVILBulkRNASeq")
config

## grab inputs and outpus
## define a new input
inputs <- avworkflow_configuration_inputs_template(config)
outputs <- avworkflow_configuration_outputs_template(config)

inputs
outputs

inputs$attribute[4] <- "\"new_index_name\""

## update current configuration with new input information

new_config <- avworkflow_configuration_update(config, inputs)
#new_config <- avworkflow_configuration_update(config, inputs, outputs)
new_config 

## next step would be to run new version of workflow
## FIXME: This doesn't work, produces 'Bad Request (HTTP 400)'
avworkflow_import_configuration(new_config)
