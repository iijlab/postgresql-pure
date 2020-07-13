PWSH = pwsh

.PHONY: build
build: build-ghc-8.4 build-ghc-8.6 build-ghc-8.8 build-nightly

.PHONY: build-ghc-8.4
build-ghc-8.4: build-deps-ghc-8.4 src
	stack --stack-yaml stack-ghc-8.4.yaml build --ghc-options -Werror

.PHONY: build-ghc-8.6
build-ghc-8.6: build-deps-ghc-8.6 src
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror

.PHONY: build-ghc-8.8
build-ghc-8.8: build-deps-ghc-8.8 src
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror

.PHONY: build-nightly
build-nightly: build-deps-nightly src
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror

.PHONY: build-deps-ghc-8.4
build-deps-ghc-8.4: stack-ghc-8.4.yaml package.yaml
	stack --stack-yaml stack-ghc-8.4.yaml build --only-dependencies

.PHONY: build-deps-ghc-8.6
build-deps-ghc-8.6: stack-ghc-8.6.yaml package.yaml
	stack --stack-yaml stack-ghc-8.6.yaml build --only-dependencies

.PHONY: build-deps-ghc-8.8
build-deps-ghc-8.8: stack-ghc-8.8.yaml package.yaml
	stack --stack-yaml stack-ghc-8.8.yaml build --only-dependencies

.PHONY: build-deps-nightly
build-deps-nightly: stack-ghc-8.8.yaml package.yaml
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --only-dependencies

.PHONY: test
test: test-ghc-8.4 test-ghc-8.6 test-ghc-8.8 test-nightly

.PHONY: test-ghc-8.4
test-ghc-8.4: test-doctest-ghc-8.4 test-original-ghc-8.4 test-hdbc-postgresql-ghc-8.4 test-relational-record-ghc-8.4

.PHONY: test-doctest-ghc-8.4
test-doctest-ghc-8.4: build-ghc-8.4
	stack --stack-yaml stack-ghc-8.4.yaml build --ghc-options -Werror postgresql-pure:test:doctest

.PHONY: test-original-ghc-8.4
test-original-ghc-8.4: build-ghc-8.4
	stack --stack-yaml stack-ghc-8.4.yaml build --ghc-options -Werror postgresql-pure:test:original

.PHONY: test-hdbc-postgresql-ghc-8.4
test-hdbc-postgresql-ghc-8.4: build-ghc-8.4
	stack --stack-yaml stack-ghc-8.4.yaml build --ghc-options -Werror postgresql-pure:test:hdbc-postgresql

.PHONY: test-relational-record-ghc-8.4
test-relational-record-ghc-8.4: build-ghc-8.4
	stack --stack-yaml stack-ghc-8.4.yaml build --ghc-options -Werror postgresql-pure:test:relational-record

.PHONY: test-ghc-8.6
test-ghc-8.6: test-doctest-ghc-8.6 test-original-ghc-8.6 test-hdbc-postgresql-ghc-8.6 test-relational-record-ghc-8.6

.PHONY: test-doctest-ghc-8.6
test-doctest-ghc-8.6: build-ghc-8.6
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror postgresql-pure:test:doctest

.PHONY: test-original-ghc-8.6
test-original-ghc-8.6: build-ghc-8.6
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror postgresql-pure:test:original

.PHONY: test-hdbc-postgresql-ghc-8.6
test-hdbc-postgresql-ghc-8.6: build-ghc-8.6
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror postgresql-pure:test:hdbc-postgresql

.PHONY: test-relational-record-ghc-8.6
test-relational-record-ghc-8.6: build-ghc-8.6
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror postgresql-pure:test:relational-record

.PHONY: test-ghc-8.8
test-ghc-8.8: test-doctest-ghc-8.8 test-original-ghc-8.8 test-hdbc-postgresql-ghc-8.8 test-relational-record-ghc-8.8

.PHONY: test-doctest-ghc-8.8
test-doctest-ghc-8.8: build-ghc-8.8
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror postgresql-pure:test:doctest

.PHONY: test-original-ghc-8.8
test-original-ghc-8.8: build-ghc-8.8
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror postgresql-pure:test:original

.PHONY: test-hdbc-postgresql-ghc-8.8
test-hdbc-postgresql-ghc-8.8: build-ghc-8.8
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror postgresql-pure:test:hdbc-postgresql

.PHONY: test-relational-record-ghc-8.8
test-relational-record-ghc-8.8: build-ghc-8.8
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror postgresql-pure:test:relational-record

.PHONY: test-nightly
test-nightly: test-doctest-nightly test-original-nightly test-hdbc-postgresql-nightly test-relational-record-nightly

.PHONY: test-doctest-nightly
test-doctest-nightly: build-nightly
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror postgresql-pure:test:doctest

.PHONY: test-original-nightly
test-original-nightly: build-nightly
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror postgresql-pure:test:original

.PHONY: test-hdbc-postgresql-nightly
test-hdbc-postgresql-nightly: build-nightly
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror postgresql-pure:test:hdbc-postgresql

.PHONY: test-relational-record-nightly
test-relational-record-nightly: build-nightly
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror postgresql-pure:test:relational-record

.PHONY: format
format:
	$(PWSH) -Command "& { Get-ChildItem -Filter '*.hs' -Recurse src, test, test-doctest, test-relational-record, benchmark | Where-Object { $$_.Directory -notlike '*\src\Database\PostgreSQL\Simple\Time\Internal' } | ForEach-Object { stack exec -- stylish-haskell -i $$_.FullName } }"
	stack exec -- stylish-haskell -i Setup.hs

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

pages-path=../postgresql-pure-pages

.PHONY: doc
doc:
	$(PWSH) -Command "& {\
		Remove-Item -Recurse $(pages-path)\*;\
		stack --stack-yaml stack-nightly.yaml haddock --haddock-arguments '--odir $(pages-path)';\
		$$revision = $$(git rev-parse HEAD);\
		Push-Location $(pages-path);\
		git add .;\
		git commit -m $$revision;\
		Pop-Location\
	}"

.PHONY: targets
targets:
	$(PWSH) -Command "& { Get-Content .\Makefile | Where-Object { $$_ -like '.PHONY*' } | ForEach-Object { $$_.Substring(8) } }"

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
