


test:
    echo "Running Tests"
    cabal test --test-option="--hide-successes"

update-version version:
    echo "Update Version: {{version}}"
    sed -i "s/Version \".*\"/Version \"{{version}}\"/g"  app/Main.hs
    sed -i "s/^version\:.*/version: {{version}}/g"  Hastructure.cabal

tag-files env version:
    echo "Tagging Files"
    git add CHANGELOG.md
    git add app/Main.hs Hastructure.cabal swagger.json
    git commit -m "bump version to-> < {{version}} >"
    git tag -a {{env}}{{version}} -m "{{env}}{{version}}"
    git push origin HEAD --tag

push-code:
    echo "Pushing Code"
    git push --tag
    git push origin HEAD

upload-chlog:
    echo "<CHANGE LOG> Upload Change Log "
    scp CHANGELOG.md root@simplicity.vip:/root/absbox.org/ChangeLog.md

