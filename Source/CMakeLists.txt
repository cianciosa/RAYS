target_sources (rays

                PRIVATE

                $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/backtrace.c>
                $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/assert.f90>
                $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/equilibrium_interface.f90>
)
