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
  execSync(`gcloud run services update rshiny ` +
    `--image us-docker.pkg.dev/pailim-dataset/docker/rshiny:latest ` +
    `--timeout=1800 ` +
    `--concurrency=1000 ` +
    `--session-affinity ` + // vs --no-session-affinity
    `--min-instances=0 ` +
    `--max-instances=3`, { stdio: 'inherit' }); // 30 min timeout, 1000 concurrent requests per instance, 0-3 instances

  // Delete all existing containers except :latest
  console.log('🗑️ Deleting old containers (keeping :latest)...');
  try {
    // Get list of all images with their tags
    const listCommand = 'gcloud artifacts docker images list us-docker.pkg.dev/pailim-dataset/docker/rshiny --format="value(IMAGE,TAGS)"';
    const imageList = execSync(listCommand, { encoding: 'utf8' }).trim();
    
    if (imageList) {
      const lines = imageList.split('\n').filter(line => line.trim());
      const imagesToDelete = [];
      
      for (const line of lines) {
        const [image, tags] = line.split('\t');
        // Only delete images that don't have the 'latest' tag
        if (tags && !tags.includes('latest')) {
          imagesToDelete.push(image.trim());
        }
      }
      
      if (imagesToDelete.length > 0) {
        for (const image of imagesToDelete) {
          execSync(`gcloud artifacts docker images delete ${image} --quiet`, { stdio: 'inherit' });
        }
        console.log(`✅ Deleted ${imagesToDelete.length} old containers (kept :latest)`);
      } else {
        console.log('ℹ️  No old containers found to delete (keeping :latest)');
      }
    } else {
      console.log('ℹ️  No containers found in repository');
    }
  } catch (error) {
    console.log('ℹ️  Error cleaning up old containers, continuing deployment...');
  }

  console.log('✅ Deployment completed successfully!');
} catch (error) {
  console.error('❌ Deployment failed:', error.message);
  process.exit(1);
}
