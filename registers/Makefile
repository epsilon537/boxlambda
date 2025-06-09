all: build

MODULES := uart reset

build: $(MODULES)
	@$(foreach dep,$^,python3 -m corsair -c definitions/$(dep).config -r definitions/$(dep).yaml;)

clean:
	@rm -rf generated

.PHONY: all clean $(MODULES)
