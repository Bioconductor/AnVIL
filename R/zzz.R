.onLoad <-
    function(...)
{
    tryCatch(anvil_options("leonardo_access"), error = warning)
}
