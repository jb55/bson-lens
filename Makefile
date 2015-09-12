
publish-docs:
	rsync -avzP result/share/doc/*/bson-lens*/html/ jb55.com:public/docs/bson-lens/
