# call like Invoke-psake -Task Build -parameters @{'ghc' = 'all'; 'test' = $true}
Task Build {
    if ($ghc -eq 'all') { $ghc = @('8.4', '8.6', '8.8') }
    else { if ($ghc -eq $null) { $ghc = @('8.6') } }
    foreach ($c in $ghc) {
        switch ($c) {
            '8.4' { $stack_args = @('--stack-yaml', 'stack-ghc-8.4.yaml') }
            '8.6' { $stack_args = @('--stack-yaml', 'stack-ghc-8.6.yaml') }
            '8.8' { $stack_args = @('--stack-yaml', 'stack-ghc-8.8.yaml') }
            'nightly' { $stack_args = @('--stack-yaml', 'stack-ghc-8.8.yaml', '--resolver', 'nightly') }
            default { Write-Error "Unexpected GHC: $c"}
        }
        if ($test -eq $null) {
            Write-Host "stack $stack_args build --ghc-options -Werror"
            Exec { stack $stack_args build --ghc-options -Werror }
        }
        else {
            if ($test.GetType() -eq [string] -and $test -eq 'all') { $test = @('doctest', 'hdbc-postgresql', 'original', 'relational-record') }
            else { if ($test.GetType() -eq [boolean] -and $test -eq $true) { $test = @('doctest', 'hdbc-postgresql', 'original') } }
            foreach ($t in $test) {
                Write-Host "stack $stack_args test --ghc-options -Werror postgresql-pure:test:$t"
                Exec { stack $stack_args test --ghc-options -Werror postgresql-pure:test:$t }
            }
        }
    }
}

Task Format {
    Exec { Get-ChildItem -Filter '*.hs' -Recurse src, test, test-doctest, test-relational-record, benchmark | Where-Object { $_.Directory -notlike '*\src\Database\PostgreSQL\Simple\Time\Internal' } | ForEach-Object { stack exec -- stylish-haskell -i $_.FullName } }
}

Task Lint {
    Exec {
        stack exec -- hlint `
          src/Database/PostgreSQL/Pure.hs `
          src/Database/PostgreSQL/Pure `
          src/Database/HDBC `
          test `
          test-doctest `
          test-relational-record `
          benchmark
    }
}
