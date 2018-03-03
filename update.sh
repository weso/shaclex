# set oldversion=0.0.0
# set version=0.0.1

# set source=\src\shapes\shaclex\target\universal
# set dest=\gh-pages\shaclex\binaries

# rm %dest%\shaclex-%oldversion%.zip
# rm %dest%\shaclex-%oldversion%.tgz
# cp %source%\shaclex-%version%.zip %dest%\shaclex-%version%.zip
# cp %source%\shaclex-%version%.tgz %dest%\shaclex-%version%.tgz
# sed -i "s|%oldversion%|%version%|g" index.html
# chmod +w index.html
rm -rf latest/api/*
cp -R ~/src/shapes/shaclex/target/scala-2.12/api/* latest/api
git add -A
git commit -m "Updated API"
git push
