cmake_minimum_required(VERSION 4.0)
project(std C)

set(CMAKE_C_STANDARD 11)

include_directories(build)

add_executable(std
        build/std.c
        build/std.h)
