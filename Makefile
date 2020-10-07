
login:
	aws ecr get-login-password --region us-east-2 | docker login --username AWS --password-stdin 423272453964.dkr.ecr.us-east-2.amazonaws.com

build:
	docker build -t vaccine-efficacy .
	docker tag vaccine-efficacy:latest 423272453964.dkr.ecr.us-east-2.amazonaws.com/vaccine-efficacy:latest

push: login
	docker push 423272453964.dkr.ecr.us-east-2.amazonaws.com/vaccine-efficacy:latest