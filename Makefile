.PHONY: build
build:
	make pre-build

	go install cmd/pasc/pasc.go
	
	cmake -G "Unix Makefiles" -B pkg/codegen/backend/build -S pkg/codegen/backend
	cmake --build pkg/codegen/backend/build --target install --config Release

test:
	go test ./... -v

deps:
	echo "Installing dependencies..."

pre-build:
	protoc --proto_path=${PWD}/pkg --go_out="." ${PWD}/pkg/proto/token.proto ${PWD}/pkg/proto/program.proto ${PWD}/pkg/proto/expression.proto ${PWD}/pkg/proto/statement.proto ${PWD}/pkg/proto/type.proto
	protoc --proto_path=${PWD}/pkg --cpp_out="pkg/codegen/backend/include" ${PWD}/pkg/proto/token.proto ${PWD}/pkg/proto/program.proto ${PWD}/pkg/proto/expression.proto ${PWD}/pkg/proto/statement.proto ${PWD}/pkg/proto/type.proto
