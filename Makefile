PWSH = pwsh

.PHONY: build
build: build-8.4 build-8.6 build-8.8 build-nightly

.PHONY: build-8.4
build-8.4: build-deps-8.4 src
	stack --stack-yaml stack-ghc-8.4.yaml build --ghc-options -Werror

.PHONY: build-8.6
build-8.6: build-deps-8.6 src
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror

.PHONY: build-8.8
build-8.8: build-deps-8.8 src
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror

.PHONY: build-nightly
build-nightly: build-deps-nightly src
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror

.PHONY: build-deps-8.4
build-deps-8.4: stack-ghc-8.4.yaml package.yaml
	stack --stack-yaml stack-ghc-8.4.yaml build --only-dependencies

.PHONY: build-deps-8.6
build-deps-8.6: stack-ghc-8.6.yaml package.yaml
	stack --stack-yaml stack-ghc-8.6.yaml build --only-dependencies

.PHONY: build-deps-8.8
build-deps-8.8: stack-ghc-8.8.yaml package.yaml
	stack --stack-yaml stack-ghc-8.8.yaml build --only-dependencies

.PHONY: build-deps-nightly
build-deps-nightly: stack-ghc-8.8.yaml package.yaml
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --only-dependencies

.PHONY: test
test: test-8.4 test-8.6 test-8.8 test-nightly

.PHONY: test-8.4
test-8.4: test-doctest-8.4 test-original-8.4 test-hdbc-postgresql-8.4 test-relational-record-8.4

.PHONY: test-doctest-8.4
test-doctest-8.4: build-8.4
	stack --stack-yaml stack-ghc-8.4.yaml build --ghc-options -Werror postgresql-pure:test:doctest

.PHONY: test-original-8.4
test-original-8.4: build-8.4
	stack --stack-yaml stack-ghc-8.4.yaml build --ghc-options -Werror postgresql-pure:test:original

.PHONY: test-hdbc-postgresql-8.4
test-hdbc-postgresql-8.4: build-8.4
	stack --stack-yaml stack-ghc-8.4.yaml build --ghc-options -Werror postgresql-pure:test:hdbc-postgresql

.PHONY: test-relational-record-8.4
test-relational-record-8.4: build-8.4
	stack --stack-yaml stack-ghc-8.4.yaml build --ghc-options -Werror postgresql-pure:test:relational-record

.PHONY: test-8.6
test-8.6: test-doctest-8.6 test-original-8.6 test-hdbc-postgresql-8.6 test-relational-record-8.6

.PHONY: test-doctest-8.6
test-doctest-8.6: build-8.6
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror postgresql-pure:test:doctest

.PHONY: test-original-8.6
test-original-8.6: build-8.6
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror postgresql-pure:test:original

.PHONY: test-hdbc-postgresql-8.6
test-hdbc-postgresql-8.6: build-8.6
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror postgresql-pure:test:hdbc-postgresql

.PHONY: test-relational-record-8.6
test-relational-record-8.6: build-8.6
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror postgresql-pure:test:relational-record

.PHONY: test-8.8
test-8.8: test-doctest-8.8 test-original-8.8 test-hdbc-postgresql-8.8 test-relational-record-8.8

.PHONY: test-doctest-8.8
test-doctest-8.8: build-8.8
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror postgresql-pure:test:doctest

.PHONY: test-original-8.8
test-original-8.8: build-8.8
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror postgresql-pure:test:original

.PHONY: test-hdbc-postgresql-8.8
test-hdbc-postgresql-8.8: build-8.8
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror postgresql-pure:test:hdbc-postgresql

.PHONY: test-relational-record-8.8
test-relational-record-8.8: build-8.8
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror postgresql-pure:test:relational-record

.PHONY: test-nightly
test-nightly: test-doctest-nightly test-original-nightly test-hdbc-postgresql-nightly test-relational-record-nightly

.PHONY: test-doctest-nightly
test-doctest:-nightly build-nightly
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror postgresql-pure:test:doctest

.PHONY: test-original-nightly
test-original:-nightly build-nightly
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror postgresql-pure:test:original

.PHONY: test-hdbc-postgresql-nightly
test-hdbc-postgresql:-nightly build-nightly
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror postgresql-pure:test:hdbc-postgresql

.PHONY: test-relational-record-nightly
test-relational-record:-nightly build-nightly
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror postgresql-pure:test:relational-record

.PHONY: format
format:
	$(PWSH) -Command "& { Get-ChildItem -Filter '*.hs' -Recurse src, test, test-doctest, test-relational-record, benchmark | Where-Object { $$_.Directory -notlike '*\src\Database\PostgreSQL\Simple\Time\Internal' } | ForEach-Object { stack exec -- stylish-haskell -i $$_.FullName } }"

.PHONY: lint
lint:
	stack exec -- hlint\
		src/Database/PostgreSQL/Pure.hs\
		src/Database/PostgreSQL/Pure\
		src/Database/HDBC\
		test\
		test-doctest\
		test-relational-record\
		benchmark

.PHONY: clean
clean:
	stack --stack-yaml stack-ghc-8.4.yaml clean
	stack --stack-yaml stack-ghc-8.6.yaml clean
	stack --stack-yaml stack-ghc-8.8.yaml clean

.PHONY: clean-full
clean-full:
	stack --stack-yaml stack-ghc-8.4.yaml clean --full
	stack --stack-yaml stack-ghc-8.6.yaml clean --full
	stack --stack-yaml stack-ghc-8.8.yaml clean --full
