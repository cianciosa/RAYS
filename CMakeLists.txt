cmake_minimum_required (VERSION 3.22)

project (rays C Fortran)

#-------------------------------------------------------------------------------
#  Setup build types.
#-------------------------------------------------------------------------------
set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
              Debug
              Release
              Sanitized
)

#-------------------------------------------------------------------------------
#  Sanitizer options
#-------------------------------------------------------------------------------
macro (register_sanitizer_option name default)
    string (TOUPPER ${name} upper_name)

    option (SANITIZE_${upper_name} "Enable the ${name} sanitizer" ${default})
endmacro ()

register_sanitizer_option (address   ON)
register_sanitizer_option (leak      OFF)
register_sanitizer_option (memory    OFF)
register_sanitizer_option (thread    OFF)
register_sanitizer_option (undefined ON)

#-------------------------------------------------------------------------------
#  Set up rays library target.
#-------------------------------------------------------------------------------

add_library (rays)
target_compile_features (rays
                         PUBLIC
                         c_std_99
)
target_compile_definitions (rays
                            PUBLIC
                            BACKTRACE_SIZE=128
)
target_compile_options (rays
                        PUBLIC
                        $<$<CONFIG:Sanitized>:$<$<BOOL:${SANITIZE_ADDRESS}>:-fsanitize=address>>
                        $<$<CONFIG:Sanitized>:$<$<BOOL:${SANITIZE_THREAD}>:-fsanitize=thread>>
                        $<$<CONFIG:Sanitized>:$<$<BOOL:${SANITIZE_MEMORY}>:-fsanitize=memory>>
                        $<$<CONFIG:Sanitized>:$<$<BOOL:${SANITIZE_UNDEFINED}>:-fsanitize=undefined>>
                        $<$<CONFIG:Sanitized>:$<$<BOOL:${SANITIZE_LEAK}>:-fsanitize=leak>>
                        $<$<CONFIG:Sanitized>:-g>
                        $<$<COMPILE_LANGUAGE:Fortran>:-cpp>
)
target_link_options (rays
                     PUBLIC
                     $<$<CONFIG:Sanitized>:$<$<BOOL:${SANITIZE_ADDRESS}>:-fsanitize=address>>
                     $<$<CONFIG:Sanitized>:$<$<BOOL:${SANITIZE_THREAD}>:-fsanitize=thread>>
                     $<$<CONFIG:Sanitized>:$<$<BOOL:${SANITIZE_MEMORY}>:-fsanitize=memory>>
                     $<$<CONFIG:Sanitized>:$<$<BOOL:${SANITIZE_UNDEFINED}>:-fsanitize=undefined>>
                     $<$<CONFIG:Sanitized>:$<$<BOOL:${SANITIZE_LEAK}>:-fsanitize=leak>>
)

add_subdirectory (Source)

#-------------------------------------------------------------------------------
#  Setup testing
#-------------------------------------------------------------------------------
enable_testing ()
