package sim8088

import "core:os"
import "core:mem"
_ :: mem
import "core:fmt"
import "core:flags"

Flags :: struct {
    file: os.Handle `args:"pos=0,required,file=r" usage:"Input file."`,
}

main :: proc() {
    when ODIN_DEBUG {
		track: mem.Tracking_Allocator
		mem.tracking_allocator_init(&track, context.allocator)
		context.allocator = mem.tracking_allocator(&track)

		defer {
			if len(track.allocation_map) > 0 {
				fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
				for _, entry in track.allocation_map {
					fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
				}
			}
			mem.tracking_allocator_destroy(&track)
		}
	}


    f: Flags
    flags.parse_or_exit(&f, os.args, .Odin)
    s := os.stream_from_handle(f.file)

    instructions := make([dynamic]Instruction)
    defer delete(instructions)

    for i, err := instruction_from_stream(s);; i, err = instruction_from_stream(s) {
        if err != nil {
            if err != .Eof {
                fmt.println("err:", err)
            }
            break
        }
        append(&instructions, i)
    }


    fmt.println("bits 16")
    for i in instructions {
        fmt.println(instruction_string(i, allocator = context.temp_allocator))
    }
    free_all(context.temp_allocator)
}
