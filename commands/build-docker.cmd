gcloud config set account "bryce@brycechamberlainllc.com"
gcloud config set project pailim-dataset
gcloud builds submit . --dir=. --tag us-docker.pkg.dev/pailim-dataset/docker/rshiny
