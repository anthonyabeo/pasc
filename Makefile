.PHONY: build
build:
	make pre-build

	go install cmd/pasc/pasc.go
	
	cmake -G "Unix Makefiles" -B pkg/codegen/backend/build -S pkg/codegen/backend
	cmake --build pkg/codegen/backend/build --target install --config Release

test:
	go test ./... -v

deps:
	echo "Installing LLVM..."
	git clone --depth=1 https://github.com/llvm/llvm-project.git
	cd llvm-project
	mkdir build && cd build
	cmake -G "Unix Makefiles" ../llvm -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=on -DLLVM_TARGETS_TO_BUILD="X86;AArch64"

	make -j 8
	sudo make install

	echo "Protocol buffers..."
	go install google.golang.org/protobuf/cmd/protoc-gen-go@v1.28

	curl -OL https://github.com/protocolbuffers/protobuf/releases/download/v3.6.1/protobuf-cpp-3.6.1.tar.gz
	tar -xvf protobuf-cpp-3.6.1.tar.gz
	cd protobuf-cpp-3.6.1
	./configure
	make
	make check
	sudo make install
	sudo ldconfig
	protoc --version

pre-build:
	protoc --proto_path=$(shell pwd)/pkg --go_out="." $(shell pwd)/pkg/proto/token.proto $(shell pwd)/pkg/proto/program.proto $(shell pwd)/pkg/proto/expression.proto $(shell pwd)/pkg/proto/statement.proto $(shell pwd)/pkg/proto/type.proto
	protoc --proto_path=$(shell pwd)/pkg --cpp_out="pkg/codegen/backend/include" $(shell pwd)/pkg/proto/token.proto $(shell pwd)/pkg/proto/program.proto $(shell pwd)/pkg/proto/expression.proto $(shell pwd)/pkg/proto/statement.proto $(shell pwd)/pkg/proto/type.proto
