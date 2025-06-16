package sim8088

import "core:io"
import "core:fmt"
import "core:strings"

Decode_Instruction_Error :: enum {
    None,
    Eof,
    Missing_Data,
    Unsupported_Opcode,
}

Instruction_Direction :: enum u8 {
    Reg_Source      = 0, // Reg is the source, also: mov any, reg
    Reg_Destination = 1, // Reg is the destination, also: mov reg, any
}

Instruction_Size :: enum u8 {
    Byte = 0, // Byte if wide = 0
    Word = 1, // Word if wide = 1
}

// Not allocated
instruction_size_string :: proc(size: Instruction_Size) -> string {
    switch size {
    case .Byte:
        return "byte"
    case .Word:
        return "word"
    }
    unreachable()
}

Sized_Instruction_Data :: union #no_nil {
    u8,
    u16,
}

sized_instruction_data_string :: proc(
        data: Sized_Instruction_Data,
        allocator := context.allocator
    ) -> string {
    switch data in data {
    case u8:
        return fmt.aprintf("byte %d", transmute(i8)data, allocator = allocator)
    case u16:
        return fmt.aprintf("word %d", data, allocator = allocator)
    case:
        panic("unreachable")
    }
    panic("unreachable")
}

sized_instruction_data_from_stream :: proc(
        size: Instruction_Size,
        s:    io.Reader,
    ) -> (idata: Sized_Instruction_Data, err: Decode_Instruction_Error) {
    buffer: [1]u8
    _, read_err := io.read(s, buffer[:])
    if err != nil {
        err = .Missing_Data
        return
    }

    data := buffer[0]
    if size == .Word {
        // Read high byte
        _, read_err := io.read(s, buffer[:])
        if err != nil {
            err = .Missing_Data
            return
        }
        idata = u16(data) | (u16(buffer[0]) << 8)
    } else {
        idata = data
    }
    return
}

Byte_Register :: enum u8 {
    Al, Cl, Dl, Bl, Ah, Ch, Dh, Bh,
}

Word_Register :: enum u8 {
    Ax, Cx, Dx, Bx, Sp, Bp, Si, Di,
}

Register :: enum uint {
    Al, Cl, Dl, Bl, Ah, Ch, Dh, Bh,
    Ax, Cx, Dx, Bx, Sp, Bp, Si, Di,
}

register_string :: proc(reg: Register, allocator := context.allocator) \
    -> string {
    ada_case := fmt.aprintf("%v", reg, allocator = allocator)
    defer delete(ada_case, allocator = allocator)
    return strings.to_lower(ada_case, allocator = allocator)
}

register_from_byte_register :: proc(reg: Byte_Register) -> Register {
    return Register(reg)
}

register_from_word_register :: proc(reg: Word_Register) -> Register {
    return Register(reg) + Register.Ax
}

register_from_byte_with_size :: proc(b: u8, size: Instruction_Size) -> Register {
    switch size {
    case .Byte:
        return register_from_byte_register(Byte_Register(b))
    case .Word:
        return register_from_word_register(Word_Register(b))
    case:
        unreachable()
    }
}

Instruction_Memory_Mode_Kind :: enum u8 {
    Bx_Si,
    Bx_Di,
    Bp_Si,
    Bp_Di,
    Si,
    Di,
    Direct_Address_Or_Bp,
    Bx,
}

Instruction_Memory_Mode :: struct {
    kind:           Instruction_Memory_Mode_Kind,
    direct_address: bool, // If kind == Direct_Address_Or_Bp, and this is true, then data contains the direct address
    data:           u16,
}

#assert(size_of(Instruction_Memory_Mode) == 4)

Instruction_Displacement :: union #no_nil {
    Instruction_Memory_Mode,
    Register, // Register mode
}

instruction_displacement_string :: proc(id: Instruction_Displacement, allocator := context.allocator) -> string {
    switch id in id {
    case Register:
        return register_string(id, allocator = allocator)
    case Instruction_Memory_Mode:
        displacement: string
        switch id.kind {
            case .Bx_Si: displacement = "bx + si"
            case .Bx_Di: displacement = "bx + di"
            case .Bp_Si: displacement = "bp + si"
            case .Bp_Di: displacement = "bp + di"
            case .Si:    displacement = "si"
            case .Di:    displacement = "di"
            case .Direct_Address_Or_Bp:
                if id.direct_address {
                    return fmt.aprintf("[%d]", id.data, allocator = allocator)
                }
                displacement = "bp"
            case .Bx:    displacement = "bx"
        }
        if id.data == 0 {
            return fmt.aprintf("[%s]", displacement, allocator = allocator)
        } else {
            return fmt.aprintf("[%s + %d]", displacement, transmute(i16)id.data, allocator = allocator)
        }
    case:
        unreachable()
    }
}

instruction_displacement_from_stream :: proc(
        size: Instruction_Size,
        s: io.Reader
    ) -> (id: Instruction_Displacement, read_byte: u8, err: Decode_Instruction_Error) {
    buffer: [1]u8
    _, read_err := io.read(s, buffer[:])
    if read_err != nil {
        err = .Missing_Data
        return
    }

    read_byte = buffer[0]

    mode := read_byte >> 6
    rm := read_byte & 0b111

    switch mode {
    case 0:
        mem_mode := Instruction_Memory_Mode_Kind(rm)
        data: u16
        if mem_mode == .Direct_Address_Or_Bp {
            buffer2: [2]u8
            _, read_err = io.read(s, buffer2[:])
            if read_err != nil {
                err = .Missing_Data
                return
            }

            data  = u16(buffer2[0])
            data |= u16(buffer2[1]) << 8
        }
        id = Instruction_Memory_Mode{kind = mem_mode, data = data, direct_address = true}
        return
    case 1:
        mem_mode := Instruction_Memory_Mode_Kind(rm)

        _, read_err := io.read(s, buffer[:])
        if read_err != nil {
            err = .Missing_Data
            return
        }
        data := buffer[0]

        id = Instruction_Memory_Mode{
            kind = mem_mode,
            data = transmute(u16) (i16( transmute(i8) data ))
        }
        return
    case 2:
        mem_mode := Instruction_Memory_Mode_Kind(rm)
        buffer2: [2]u8
        _, read_err = io.read(s, buffer2[:])
        if read_err != nil {
            err = .Missing_Data
            return
        }

        data := u16(buffer2[0])
        data |= u16(buffer2[1]) << 8

        id = Instruction_Memory_Mode{
            kind = mem_mode,
            data = data,
        }
        return
    case 3:
        id = register_from_byte_with_size(rm, size)
        return
    case:
        unreachable()
    }
}

Acc_Mem :: struct {
    size: Instruction_Size,
    addr: u16,
}

Mov_Reg_Mem_To_From_Reg :: struct {
    direction:    Instruction_Direction,
    size:         Instruction_Size,
    reg:          Register,
    displacement: Instruction_Displacement,
}

Mov_Imm_To_Reg_Mem :: struct {
    sized_data:   Sized_Instruction_Data,
    displacement: Instruction_Displacement,
}

Mov_Imm_To_Reg :: struct {
    sized_data: Sized_Instruction_Data,
    reg:        Register,
}

Mov_Mem_To_Acc :: distinct Acc_Mem
Mov_Acc_To_Mem :: distinct Acc_Mem

Mov_Instruction :: union #no_nil { Mov_Reg_Mem_To_From_Reg, Mov_Imm_To_Reg_Mem, Mov_Imm_To_Reg, Mov_Mem_To_Acc, Mov_Acc_To_Mem }

Instruction :: union { Mov_Instruction }

instruction_string :: proc(i: Instruction, allocator := context.allocator) -> string {
    switch i in i {
    case Mov_Instruction:
        switch mov in i {
        case Mov_Reg_Mem_To_From_Reg:
            d, r := instruction_displacement_string(mov.displacement, allocator = allocator), register_string(mov.reg, allocator = allocator)
            defer { delete(d, allocator = allocator); delete(r, allocator = allocator) }
            switch mov.direction {
            case .Reg_Source:
                return fmt.aprintf("mov %s, %s", d, r, allocator = allocator)
            case .Reg_Destination:
                return fmt.aprintf("mov %s, %s", r, d, allocator = allocator)
            case: unreachable()
            }
        case Mov_Imm_To_Reg_Mem:
            di, da := instruction_displacement_string(mov.displacement, allocator = allocator), sized_instruction_data_string(mov.sized_data, allocator = allocator)
            defer { delete(di, allocator = allocator); delete(da, allocator = allocator) }
            return fmt.aprintf("mov %s, %s", di, da, allocator = allocator)
        case Mov_Imm_To_Reg:
            r, d := register_string(mov.reg, allocator = allocator), sized_instruction_data_string(mov.sized_data, allocator = allocator)
            defer { delete(r, allocator = allocator); delete(d, allocator = allocator) }
            return fmt.aprintf("mov %s, %s", r, d, allocator = allocator)
        case Mov_Mem_To_Acc:
            return fmt.aprintf("mov ax, [%s %d]", instruction_size_string(mov.size), mov.addr, allocator = allocator)
        case Mov_Acc_To_Mem:
            return fmt.aprintf("mov [%s %d], ax", instruction_size_string(mov.size), mov.addr, allocator = allocator)
        }
    case:
        fmt.panicf("nil instruction not supported")
    }
    unreachable()
}

instruction_from_stream :: proc(s: io.Reader) -> (i: Instruction, err: Decode_Instruction_Error) {
    opcode, rerr := io.read_byte(s)
    if rerr != nil {
        err = .Eof
        return
    }

    switch opcode {
    case 0o210..=0o213: // reg/men to/from reg
        direction := Instruction_Direction((opcode >> 1) & 0b1)
        size := Instruction_Size(opcode & 0b1)
        displacement, read_byte := instruction_displacement_from_stream(size, s) or_return
        reg := register_from_byte_with_size((read_byte >> 3) & 0b111, size)
        return Mov_Reg_Mem_To_From_Reg{
            direction = direction,
            size = size,
            displacement = displacement,
            reg = reg,
        }, nil
    case 0o306, 0o307: // imm to mem
        size := Instruction_Size(opcode & 0b1)
        displacement, _ := instruction_displacement_from_stream(size, s) or_return
        sized_data := sized_instruction_data_from_stream(size, s) or_return
        return Mov_Imm_To_Reg_Mem{sized_data = sized_data, displacement = displacement}, nil
    case 0o260..=0o277: // imm to reg
        size := Instruction_Size((opcode >> 3) & 0b1)
        reg := register_from_byte_with_size(opcode & 0b111, size)
        sized_data := sized_instruction_data_from_stream(size, s) or_return
        return Mov_Imm_To_Reg{reg = reg, sized_data = sized_data}, nil
    case 0o240, 0o241:  // mem to acc
        size := Instruction_Size(opcode & 0b1)
        buffer: [2]u8
        _, read_err := io.read(s, buffer[:])
        if read_err != nil {
            err = .Missing_Data
            return
        }
        return Mov_Mem_To_Acc{size = size, addr = transmute(u16)buffer}, nil
    case 0o242, 0o243: // acc to mem
        size := Instruction_Size(opcode & 0b1)
        buffer: [2]u8
        _, read_err := io.read(s, buffer[:])
        if read_err != nil {
            err = .Missing_Data
            return
        }
        return Mov_Acc_To_Mem{size = size, addr = transmute(u16)buffer}, nil
    }

    err = .Unsupported_Opcode
    return
}
