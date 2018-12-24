.onAttach <-
    function(...)
{
    tryCatch(anvil("leonardo_access"), error = warning)
}
