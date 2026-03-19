# TODO use mise?
.PHONY: release clean nix-develop

release:
	@nix --extra-experimental-features 'nix-command flakes' develop --command bunx shadow-cljs release app

nix-develop:
	@nix --extra-experimental-features 'nix-command flakes' develop

clean:
	@rm -f main.js
