context("Partitioning")

test_that("Partitioning",{
	
	seqs<-read.table(system.file("extdata", "cjun.txt", package = "DepLogo"),stringsAsFactors = FALSE)
	data<-DLData(sequences = seqs[,1], weights = log1p( seqs[,2] ) )
	
	parts<-partition(data,threshold = 0,minElements = 1000)
	expect_equal(length(parts),5)
	
	for(el in parts){
		expect_gte(length(el),1000)
	}
	
	parts<-partition(data,threshold = 0,minElements = length(data))
	expect_equal(length(parts),1)
	
	parts<-partition(data,threshold = 0, maxNum = 1,minElements = 1)
	expect_lte(length(parts),16)
	
	parts<-partition(data)
	num<-apply(parts[[1]]$data,2,function(a){length(unique(a))})
	expect(min(num),1)
})