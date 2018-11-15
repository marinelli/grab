
#### An exercise in grabbing data from an HTML page

HOWTO:

```
$ stack build
$ zcat tv.html.gz | stack exec grab-exe - > result.json
```

or

```
$ stack exec grab-exe tv.html > result.json
```

