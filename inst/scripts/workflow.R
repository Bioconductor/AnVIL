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
outputs <- avworkflow_configuration_oupts_template(config)

inputs
outputs

inputs$attribute[4] <- "\"new_index_name\""

## update current configuration with new input information
updated_inputs <- setNames(as.list(inputs$attribute), inputs$name)
updated_outputs <- setNames(as.list(outputs$attribute), outputs$name)

updated_inputs
updated_outputs

new_config <- avworkflow_configuration_update(config, updated_inputs, updated_outputs)
new_config 
## FIXME: Not sure how to handle if something isn't updated, i.e. the outputs 
## in this example. I had to create an updated outputs variable even though 
## nothing was updated, needed the outputs to be a named list, not a data frame.

## next step would be to run new version of workflow
## FIXME: This doesn't work, produces 'Bad Request (HTTP 400)'
avworkflow_import_configuration(new_config)
