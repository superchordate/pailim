gcloud config set account "bryce@brycechamberlainllc.com"
gcloud config set project pailim-dataset
python -c "import shutil; shutil.make_archive('app', 'zip', '.', 'app')"
gsutil cp app.zip gs://import-loud-galaxy/app.zip
gcloud run services update rshiny --image us-docker.pkg.dev/pailim-dataset/docker/rshiny:latest
