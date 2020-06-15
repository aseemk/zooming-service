#!/bin/bash
set -euo pipefail

SHA1=$1

# Push image to ECR
$(aws ecr get-login --region us-east-2 --no-include-email)
docker push $ZH_AWS_ACCOUNT_ID.dkr.ecr.us-east-2.amazonaws.com/$ZH_AWS_ECR_REPO:$SHA1

# Create new Elastic Beanstalk version
EB_BUCKET=$ZH_AWS_EB_PROJECT-deploy-bucket
DOCKERRUN_FILE=$SHA1-Dockerrun.aws.json

sed "s/<TAG>/$SHA1/ ; s/<ZH_AWS_ACCOUNT_ID>/$ZH_AWS_ACCOUNT_ID/ ; s/<ZH_AWS_ECR_REPO>/$ZH_AWS_ECR_REPO/" < Dockerrun.aws.json.template > $DOCKERRUN_FILE
aws s3 cp $DOCKERRUN_FILE s3://$EB_BUCKET/$DOCKERRUN_FILE --region us-east-2
aws elasticbeanstalk create-application-version --application-name $ZH_AWS_EB_PROJECT \
    --version-label $SHA1 --source-bundle S3Bucket=$EB_BUCKET,S3Key=$DOCKERRUN_FILE \
    --region us-east-2

# Update Elastic Beanstalk environment to new version
aws elasticbeanstalk update-environment --environment-name $ZH_AWS_EB_PROJECT-env \
    --version-label $SHA1 \
    --region us-east-2
