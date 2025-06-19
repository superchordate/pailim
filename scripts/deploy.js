const { execSync } = require('child_process');

console.log('🚀 Starting deployment process...');

try {
  // Set up gcloud configuration
  console.log('📋 Setting up gcloud configuration...');
  execSync('gcloud config set account "bryce@brycechamberlainllc.com"', { stdio: 'inherit' });
  execSync('gcloud config set project pailim-dataset', { stdio: 'inherit' });

  // Build the container with cache (Cloud Build handles caching automatically)
  console.log('🏗️  Building container with cache...');
  execSync('gcloud builds submit . --dir=. --tag us-docker.pkg.dev/pailim-dataset/docker/rshiny', { stdio: 'inherit' });

  // Update the service
  console.log('🔄 Updating service...');
  execSync('gcloud run services update rshiny --image us-docker.pkg.dev/pailim-dataset/docker/rshiny:latest', { stdio: 'inherit' });

  // Delete all existing containers except :latest
  console.log('🗑️ Deleting old containers (keeping :latest)...');
  try {
    // Get list of all images and delete everything except :latest
    const listCommand = 'gcloud artifacts docker images list us-docker.pkg.dev/pailim-dataset/docker/rshiny --format="value(IMAGE)" --filter="NOT tags:latest"';
    const imagesToDelete = execSync(listCommand, { encoding: 'utf8' }).trim();
    
    if (imagesToDelete) {
      const images = imagesToDelete.split('\n').filter(img => img.trim());
      for (const image of images) {
        execSync(`gcloud artifacts docker images delete ${image.trim()} --quiet`, { stdio: 'inherit' });
      }
      console.log('✅ Old containers deleted successfully');
    } else {
      console.log('ℹ️  No old containers found to delete');
    }
  } catch (error) {
    console.log('ℹ️  Error cleaning up old containers, continuing deployment...');
  }

  console.log('✅ Deployment completed successfully!');
} catch (error) {
  console.error('❌ Deployment failed:', error.message);
  process.exit(1);
}
