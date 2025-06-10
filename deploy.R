require(easyr)
require(rsconnect)
begin()

source('secrets', local = TRUE)
rsconnect::deployApp(appDir = 'app/', appName = 'bryce')
