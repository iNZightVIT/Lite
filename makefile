# Define TAG; e.g.:
# make push TAG=dev	# latest=mainline,dev=development
.PHONY: build run log stop push clean
build:
	docker build --tag scienceis/uoa-inzight-lite:${TAG} --build-arg GITHUB_PAT=${GITHUB_PAT} .
run: pull
	docker run --rm --name lite-${TAG} -p 3838:3838 scienceis/uoa-inzight-lite:${TAG}
log:
	docker logs -f lite-${TAG}
stop:
	docker stop lite-${TAG}
push:
	docker push scienceis/uoa-inzight-lite:${TAG}
clean:
	docker image prune
pull:
	docker pull scienceis/uoa-inzight-lite:${TAG}
