.PHONY: build run log stop push clean
TAG=latest		#latest=mainline,dev=development
build:
	docker build --tag scienceis/uoa-inzight-lite:${TAG} .
run:
	docker run --name lite-${TAG} -p 3838:3838 -d scienceis/uoa-inzight-lite:${TAG}
log:
	docker logs -f lite-${TAG}
stop:
	docker stop lite-${TAG}
push:
	docker push scienceis/uoa-inzight-lite:${TAG}
clean:
	docker image prune
