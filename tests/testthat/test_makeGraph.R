context("MakeMetaNodes")
library(plyr)
library(reshape2)
library(igraph)
library(RColorBrewer)

md<-data.frame(class=c(rep('A',4),rep('B',4)),
               subclass=paste0(c(rep('A',4),rep('B',4)),rep(c(1,2),4)),
               sample=paste0('sample',1:8),
               stringsAsFactors = FALSE)

fm<-matrix(1:200,nrow = 8,ncol = 25)
colnames(fm)<-paste0('f',1:25)
rownames(fm)<-md$sample

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
                              Type=c('class','class'),
                              stringsAsFactors = FALSE))
})

test_that('makeMetaVertex makes proper vertex table for subclass',{
  v<-makeMetaVertex('subclass',md)
  expect_s3_class(v,'data.frame')
  expect_equal(dim(v),c(4,3))
  expect_identical(v,
                   data.frame(ID=c('subclass1','subclass2','subclass3','subclass4'),
                              Name=c('A1','A2','B1','B2'),
                              Type=c('subclass','subclass','subclass','subclass'),
                              stringsAsFactors = FALSE))
})

test_that('makeMetaVertex makes proper vertex table for "feature" column',{
  md1<-md
  names(md1)[2]<-'feature'
  v<-makeMetaVertex('feature',md1)
  expect_s3_class(v,'data.frame')
  expect_equal(dim(v),c(4,3))
  expect_identical(v,
                   data.frame(ID=c('cFeature1','cFeature2','cFeature3','cFeature4'),
                              Name=c('A1','A2','B1','B2'),
                              Type=c('cFeature','cFeature','cFeature','cFeature'),
                              stringsAsFactors = FALSE))
  md2<-md
  names(md2)[2]<-'FEATURE'
  v<-makeMetaVertex('FEATURE',md2)
  expect_s3_class(v,'data.frame')
  expect_equal(dim(v),c(4,3))
  expect_identical(v,
                   data.frame(ID=c('cFeature1','cFeature2','cFeature3','cFeature4'),
                              Name=c('A1','A2','B1','B2'),
                              Type=c('cFeature','cFeature','cFeature','cFeature'),
                              stringsAsFactors = FALSE))
  md3<-md
  names(md3)[2]<-'FeATURE'
  v<-makeMetaVertex('FeATURE',md3)
  expect_s3_class(v,'data.frame')
  expect_equal(dim(v),c(4,3))
  expect_identical(v,
                   data.frame(ID=c('cFeature1','cFeature2','cFeature3','cFeature4'),
                              Name=c('A1','A2','B1','B2'),
                              Type=c('cFeature','cFeature','cFeature','cFeature'),
                              stringsAsFactors = FALSE))
  md4<-md
  names(md4)[2]<-'feaTure'
  v<-makeMetaVertex('feaTure',md4)
  expect_s3_class(v,'data.frame')
  expect_equal(dim(v),c(4,3))
  expect_identical(v,
                   data.frame(ID=c('cFeature1','cFeature2','cFeature3','cFeature4'),
                              Name=c('A1','A2','B1','B2'),
                              Type=c('cFeature','cFeature','cFeature','cFeature'),
                              stringsAsFactors = FALSE))
})

test_that('makeMetaEdges makes proper edge table',{
  v<-ldply(c('class','subclass','sample'),makeMetaVertex,md)
  e<-makeMetaEdges(1,c('class','subclass','sample'),md,v,rep(1e3,3))
  expect_s3_class(e,'data.frame')
  expect_equal(dim(e),c(4,3))
  expect_identical(e,
                   data.frame(Source=c('class1','class1','class2','class2'),
                              Target=c('subclass1','subclass2','subclass3','subclass4'),
                              value=rep(1e3,4),stringsAsFactors = FALSE))
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
  expect_equal(dim(l$vertex),c(14,4))
  expect_equal(dim(l$edges),c(12,3))
})

test_that('makeGraph makes proper graph',{
  fm<-fm[sample.int(dim(fm)[1]),] # for proper testing of md/fm matching
  g<-makeGraph(featureMatrix=fm,
                  metadata=md,
                  colOrder=c('class','subclass','sample'),
                  metaStrength = 1e3)
  expect_type(g,'list')
  expect_s3_class(g,'igraph')
  expect_equal(vcount(g),39)
  expect_equal(ecount(g),212)
})
