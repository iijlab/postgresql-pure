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
