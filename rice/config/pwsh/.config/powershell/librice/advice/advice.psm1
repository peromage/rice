### hack.psm1 -- PowerShell hacks

### Function decoration
## Behavior definitions follow Emacs "define_advice". Only function is supported
function define_advice {
    param([Parameter(Mandatory, Position=0, ParameterSetName="before")]
          [Parameter(Mandatory, Position=0, ParameterSetName="after")]
          [Parameter(Mandatory, Position=0, ParameterSetName="around")]
          [Parameter(Mandatory, Position=0, ParameterSetName="override")]
          [Parameter(Mandatory, Position=0, ParameterSetName="filter_args")]
          [Parameter(Mandatory, Position=0, ParameterSetName="filter_return")]
          [string]
          $funcname,

          [Parameter(Mandatory, Position=1, ParameterSetName="before")]
          [Parameter(Mandatory, Position=1, ParameterSetName="after")]
          [Parameter(Mandatory, Position=1, ParameterSetName="around")]
          [Parameter(Mandatory, Position=1, ParameterSetName="override")]
          [Parameter(Mandatory, Position=1, ParameterSetName="filter_args")]
          [Parameter(Mandatory, Position=1, ParameterSetName="filter_return")]
          [scriptblock]
          $advice,

          [Parameter(Mandatory, ParameterSetName="before")]
          [switch]
          $before,

          [Parameter(Mandatory, ParameterSetName="after")]
          [switch]
          $after,

          [Parameter(Mandatory, ParameterSetName="around")]
          [switch]
          $around,

          [Parameter(Mandatory, ParameterSetName="override")]
          [switch]
          $override,

          [Parameter(Mandatory, ParameterSetName="filter_args")]
          [switch]
          $filter_args,

          [Parameter(Mandatory, ParameterSetName="filter_return")]
          [switch]
          $filter_return
         )

    $function_funcname = "function:${funcname}"
    if (-not (Get-Item $function_funcname -ErrorAction SilentlyContinue)) {
        "$function_funcname does not exist."
        return
    }
    $original_func = (Get-Item $function_funcname).ScriptBlock

    ## Process advice
    if ($before) {
        Set-Item $function_funcname {
            &$advice @args
            &$original_func @args
        }.GetNewClosure()
        return
    }

    if ($after) {
        Set-Item $function_funcname {
            &$original_func @args
            &$advice @args
        }.GetNewClosure()
        return
    }

    if ($around) {
        Set-Item $function_funcname {
            &$advice $original_func @args
        }.GetNewClosure()
        return
    }

    if ($override) {
        Set-Item $function_funcname {
            &$advice @args
        }.GetNewClosure()
        return
    }

    if ($filter_args) {
        Set-Item $function_funcname {
            $result = &$advice @args
            &$original_func @result
        }.GetNewClosure()
        return
    }

    if ($filter_return) {
        Set-Item $function_funcname {
            $result = &$original_func @args
            &$advice @result
        }.GetNewClosure()
        return
    }
}

Export-ModuleMember -Function *
