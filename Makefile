.PHONY: build
build:
	make pre-build

	go install cmd/pasc/pasc.go
	
	cmake -G "Unix Makefiles" -B pkg/codegen/backend/build -S pkg/codegen/backend
	cmake --build pkg/codegen/backend/build --target install --config Release

test:
	go test ./... -v

pre-build:
	protoc -I pkg/ --go_out="." program.proto
	protoc -I pkg/ --cpp_out="pkg/codegen/backend/include" program.proto	
