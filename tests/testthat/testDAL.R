context('DAL')

#reset settings so that it understands the testing environ
#working directory is inside of the testthat dir
courseraSkCapstone::readPackageSettings(x = '../../settings.yaml',
                                        environ = 'Testing')

test_that('Random SK text functions right', {
  samp1 <- getSwiftKeyRandomSample(nrows = 100, locale = 'en_US', textType = 'twitter', seedDate = as.Date("2015-01-01"))
  samp2 <- getSwiftKeyRandomSample(nrows = 100, locale = 'en_US', textType = 'twitter', seedDate = as.Date("2015-01-01"))
  expect_equal(samp1, samp2)
  file.remove(file.path(settings$cacheFiles, 'en_US_twitter_2015-01-01_n100.txt'))
})
