DOCKER_HOST=localrepo docker kill registry
DOCKER_HOST=localrepo docker rm registry
DOCKER_HOST=localrepo docker system prune --all
DOCKER_HOST=localrepo docker volume list
DOCKER_HOST=localrepo docker run -d -p 5000:5000 --restart=always --name registry registry:2
DOCKER_HOST=localrepo docker volume list
