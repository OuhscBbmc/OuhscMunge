library(testthat)

test_that("hash -all nonmissing", {
  input     <- letters[1:10]
  salt      <- "abc123"
  expected  <- c(
    "757410ac646bebb991cdd8140891b09f2d6cbfd2dca7d6fb08c8a4c0c24af1b3",
    "afdf613dc350afa36ad903f0f1638eb76c6e370d4495feaa1441b91471e76ebd",
    "2630a2754b0df7096f44173a985092c8381120b4a60c8938ce5c6b14b5983e0c",
    "4b2502443aced3c2e1428922f0bcc1e1a0eb10d37dd7b71492f27eddf3250c77",
    "83129564e4c6d6f8410afc77f0a7ffeb7f8993450f9a703bb4954cb109fffc62",
    "83c129613e3f1707ac672eefaa710425e18298f46dc0d00fbe99fc83c6861589",
    "16f316470fc023d93d75bfda78c7cf8da159953bf10bdded825b55b914b3ea38",
    "ea93537edf56b6982c9e1cf9b76c3a5dc2d2c4fb8eecff94f26151c09f1c710b",
    "e79ce785cede8c2704800fd933e25b760547974059ae921090315adc06d54ae2",
    "69a6d80dc9a3147ca7d6f45794e1ccf7f0d6b47f0a2f6f84ca79bdcc96c673ca"
  )

  actual   <- hash_and_salt_sha_256(input, salt)

  testthat::expect_equal(actual, expected)
})

test_that("hash -unsalted", {
  input     <- letters[1:10]
  expected  <- c(
    "fb1a678ef965ad4a66c712d2161f20319091cb4e7611e1925df671018c833f72",
    "61ba1f136aebcd31977da80fae3afffe18b83691e2aa2c61f948d85085fd8e56",
    "2eea4ca3e3a6518aaffdcd5bada74dc92c3e0170ab17f7826aa1d1222f8a1ef5",
    "d2aebba0a3c95f6d03acf85bd9d41e602b92675ef4741063ebb2091e064d86cd",
    "09cab1873b8f76f0d880aa963a5bd2f7c858af261acbde2a6cdee4ce993a12b3",
    "6f1066d075c43d9d362b871d35b6bb0aba94407427e70d2bedb28570ba06ad47",
    "4b536aa57b5af5b3e1c0c2abab66b909e5595a920422d42607253631135de30a",
    "41d2e334d3bd3c1eaecb0012cbba4428908b0809acb68e7aa0b43cd660ddad9a",
    "258c9d2a5232420699ed582a1320821a85d62bb8da75d66dabf0ca9f1a1b8250",
    "827f49bc49447ce20b1d239b4b26f0be499ee5b223f981855447c9924ad3f23e"
  )

  actual_salt_missing   <- hash_and_salt_sha_256(input)
  actual_salt_blank     <- hash_and_salt_sha_256(input, salt = "")

  testthat::expect_equal(actual_salt_missing, expected)
  testthat::expect_equal(actual_salt_blank  , expected)
})

test_that("hash -all nonmissing", {
  input     <- letters[1:10]
  input[2]  <- NA_character_
  input[8]  <- NA_character_
  salt      <- "abc123"
  expected  <- c(
    "757410ac646bebb991cdd8140891b09f2d6cbfd2dca7d6fb08c8a4c0c24af1b3",
    NA_character_,
    "2630a2754b0df7096f44173a985092c8381120b4a60c8938ce5c6b14b5983e0c",
    "4b2502443aced3c2e1428922f0bcc1e1a0eb10d37dd7b71492f27eddf3250c77",
    "83129564e4c6d6f8410afc77f0a7ffeb7f8993450f9a703bb4954cb109fffc62",
    "83c129613e3f1707ac672eefaa710425e18298f46dc0d00fbe99fc83c6861589",
    "16f316470fc023d93d75bfda78c7cf8da159953bf10bdded825b55b914b3ea38",
    NA_character_,
    "e79ce785cede8c2704800fd933e25b760547974059ae921090315adc06d54ae2",
    "69a6d80dc9a3147ca7d6f45794e1ccf7f0d6b47f0a2f6f84ca79bdcc96c673ca"
  )

  actual   <- hash_and_salt_sha_256(input, salt)

  testthat::expect_equal(actual, expected)
})

test_that("hash integers", {
  input_1   <- 1:10
  input_2   <- as.character(input_1)
  salt      <- "abc123"

  expected  <- c(
    "feaf057cf954408b14bf725f5d19c13fe9ef0029f463484d7b8997f90b8482cf",
    "f3d3504a43c1873a172730d5d106d6be9c19493055f20c8489199d1a7521ca7a",
    "cbd0b3d6ba0d3628750cb1e64d867c28d1804b3836e2d854195f99cc371b5391",
    "0eca64c9fe4187c7c3884cf1e2959de30bc79ac4556626129061405afa3fab35",
    "b0a8f9366b94c0ecdbf30cc6b4b252d6452bd4c57fc50575a91b621160fb335d",
    "307f92263c2057cc3054e15a6b55dc6a27b1b2b4ddeb1aa7c5d047d1c3ad843c",
    "cb97e90d4fd773c67c6d8330b8ef2d0449a7a64e7a89b5f2f6e212239cc3a7b1",
    "6d4187bdaf6498b333868ba19f812050e8b02bef5c14edfea0dd090fdb87d1cf",
    "0259d4d977d0920d1fac6ad3c8add24a463cdfc78c75a12ad5eb790a202b5dc5",
    "12a06ff72a1870f46570aff471999cc71e06bad24fcbe437856c6c3701c733da"
  )

  actual_1   <- hash_and_salt_sha_256(input_1, salt)
  actual_2   <- hash_and_salt_sha_256(input_2, salt)

  testthat::expect_equal(actual_1, expected)
  testthat::expect_equal(actual_2, expected)
})

test_that("hash blanks", {
  input <- c("a", "", "c")

  expected  <- c(
    "fb1a678ef965ad4a66c712d2161f20319091cb4e7611e1925df671018c833f72",
    NA_character_,
    "2eea4ca3e3a6518aaffdcd5bada74dc92c3e0170ab17f7826aa1d1222f8a1ef5"
  )

  actual   <- hash_and_salt_sha_256(input)

  testthat::expect_equal(actual, expected)
})
