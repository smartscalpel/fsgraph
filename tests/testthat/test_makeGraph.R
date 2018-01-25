context("MakeMetaNodes")

md<-data.frame(class=c(rep('A',4),rep('B',4)),
               subclass=paste0(c(rep('A',4),rep('B',4)),rep(c(1,2),4)),
               sample=paste0('sample',1:8))

test_that('getMetaColOrder makes proper order',{
  o<-getMetaColOrder(md)
  expect_type(o,'character')
  expect_equal(length(o),dim(md)[2])
  expect_equal(o,c('class','subclass','sample'))
})

test_that('makeMetaVertex makes proper vertex table',{
  v<-makeMetaVertex('class',md)
  expect_s3_class(v,'data.frame')
  expect_equal(dim(v),c(2,3))
  expect_identical(v,
                   data.frame(ID=c('class1','class2'),
                          Name=c('A','B'),
                          Type=c('class','class')))
})

test_that('makeMetanode makes graph with order',{
  l<-makeMetaNodes(md,c('class','subclass','sample'))
  expect_type(l,'list')
  expect_equal(length(l),2)
  expect_equal(dim(l$vertex),c(14,3))
  expect_equal(dim(l$edges),c(12,3))
})
