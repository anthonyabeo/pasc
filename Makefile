UNAME := $(shell uname -m)
export PATH := /usr/local/go/bin/:$(PATH)

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
	mkdir llvm-project/build && cd llvm-project/build
        ifeq ($(UNAME), x86_64)
		cmake -G "Unix Makefiles" -S llvm-project/llvm -B llvm-project/build -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=on -DLLVM_TARGETS_TO_BUILD="X86"
        else
		cmake -G "Unix Makefiles" -S llvm-project/llvm -B llvm-project/build -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=on -DLLVM_TARGETS_TO_BUILD="AArch64"
        endif

	make -j 8 -C llvm-project/build
	make install -C llvm-project/build

	echo "Protocol buffers..."
	sudo GOBIN=/usr/local/bin go install google.golang.org/protobuf/cmd/protoc-gen-go@v1.28

	curl -OL https://github.com/protocolbuffers/protobuf/releases/download/v3.6.1/protobuf-cpp-3.6.1.tar.gz
	tar -xvf protobuf-cpp-3.6.1.tar.gz -C /tmp
	cd /tmp/protobuf-cpp-3.6.1 && ./configure && make && make check && sudo make install
	sudo ldconfig
	protoc --version

pre-build:
	protoc --proto_path=$(shell pwd)/pkg --go_out="." $(shell pwd)/pkg/proto/token.proto $(shell pwd)/pkg/proto/program.proto $(shell pwd)/pkg/proto/expression.proto $(shell pwd)/pkg/proto/statement.proto $(shell pwd)/pkg/proto/type.proto
	protoc --proto_path=$(shell pwd)/pkg --cpp_out="pkg/codegen/backend/include" $(shell pwd)/pkg/proto/token.proto $(shell pwd)/pkg/proto/program.proto $(shell pwd)/pkg/proto/expression.proto $(shell pwd)/pkg/proto/statement.proto $(shell pwd)/pkg/proto/type.proto
