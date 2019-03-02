context("DLData")

test_that("DLData without weights",{
	seqs<-c("ACG","ACA")
	data<-DLData(seqs)
	expect_s3_class(object = data, "DLData")
	expect_equal(data$data$weights,c(1,1))
	expect_equal(as.character(data$data[1,1]),"A")
	expect_equal(data$sortByWeights,FALSE)
	expect_equal(data$alphabet$chars,c("A","C","G","T"))
	expect_equal(dim(data),c(2,4))
	expect_equal(length(data),2)
})

test_that("DLData with weights",{
	seqs<-c("ACG","GCA")
	weights<-c(1,2)
	data<-DLData(sequences = seqs,weights = weights)
	expect_equal(data$data$weights,c(2,1))
	expect_equal(as.character(data$data[1,1]),"G")
	expect_equal(data$sortByWeights,TRUE)
	expect_equal(data$alphabet$chars,c("A","C","G","T"))
	expect_equal(dim(data),c(2,4))
	
	data<-DLData(sequences = seqs,weights = weights,sortByWeights = FALSE)
	expect_equal(data$data$weights,c(1,2))
	expect_equal(as.character(data$data[1,1]),"A")
	expect_equal(data$sortByWeights,FALSE)
	expect_equal(data$alphabet$chars,c("A","C","G","T"))
	expect_equal(dim(data),c(2,4))
})

test_that("DLData alphabet, color",{
	al<-c("A","B")
	seqs<-c("AAB","BBA")
	data<-DLData(sequences = seqs,symbols = al,colors = 1:2)
	expect_equal(data$alphabet$chars,al)
	
	expect_error(DLData(seqs,symbols=al,colors=1:3))
	
	seqs<-c("ABC","AAB")
	expect_error(DLData(seqs))
	
	expect_error(DLData(seqs,symbols=al,colors=1:2))
	
})


