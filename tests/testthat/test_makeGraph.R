context("MakeMetaNodes")
library(plyr)

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

test_that('makeMetaVertex makes proper vertex table for subclass',{
  v<-makeMetaVertex('subclass',md)
  expect_s3_class(v,'data.frame')
  expect_equal(dim(v),c(4,3))
  expect_identical(v,
                   data.frame(ID=c('subclass1','subclass2','subclass3','subclass4'),
                              Name=c('A1','A2','B1','B2'),
                              Type=c('subclass','subclass','subclass','subclass')))
})

test_that('makeMetaEdges makes proper edge table',{
  v<-ldply(c('class','subclass','sample'),makeMetaVertex,md)
  e<-makeMetaEdges(1,c('class','subclass','sample'),md,v,rep(1e3,3))
  expect_s3_class(e,'data.frame')
  expect_equal(dim(e),c(4,3))
  expect_identical(e,
                   data.frame(Source=c('class1','class1','class2','class2'),
                              Target=c('subclass1','subclass2','subclass3','subclass4'),
                              value=rep(1e3,4)))
})

test_that('makeMetaEdges gives warning when outside of boundary',{
  v<-makeMetaVertex('subclass',md)
  expect_warning(makeMetaEdges(3,c('class','subclass','sample'),md,v,rep(1e3,3)),'Index is outside of the column')
  expect_warning(makeMetaEdges(4,c('class','subclass','sample'),md,v,rep(1e3,3)),'Index is outside of the column')
})


test_that('makeMetanode makes graph with order',{
  l<-makeMetaNodes(md,c('class','subclass','sample'))
  expect_type(l,'list')
  expect_equal(length(l),2)
  expect_equal(dim(l$vertex),c(14,3))
  expect_equal(dim(l$edges),c(12,3))
})
