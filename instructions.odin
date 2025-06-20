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

Imm_To_Acc :: struct {
    sized_data: Sized_Instruction_Data,
}

@private
imm_to_acc_string :: proc(opcode: string, ita: Imm_To_Acc, allocator := context.allocator) -> string {
    switch data in ita.sized_data {
    case u8:
        return fmt.aprintf("%s al, %d", opcode, data, allocator = allocator)
    case u16:
        return fmt.aprintf("%s ax, %d", opcode, data, allocator = allocator)
    case:
        unreachable()
    }
}

@private
imm_to_acc :: proc(opcode: u8, s: io.Reader) -> (i: Imm_To_Acc, err: Decode_Instruction_Error) {
    size := Instruction_Size(opcode & 0b1)
    sized_data := sized_instruction_data_from_stream(size, s) or_return
    return {sized_data}, nil
}

Imm_To_Reg_Mem :: struct {
    sized_data:   Sized_Instruction_Data,
    displacement: Instruction_Displacement,
}

@private
imm_to_reg_mem_string :: proc(opcode: string, itrm: Imm_To_Reg_Mem, allocator := context.allocator) -> string {
    di, da := instruction_displacement_string(itrm.displacement, allocator = allocator), sized_instruction_data_string(itrm.sized_data, allocator = allocator)
    defer { delete(di, allocator = allocator); delete(da, allocator = allocator) }
    return fmt.aprintf("%s %s, %s", opcode, di, da, allocator = allocator)
}

@private
imm_to_reg_mem :: proc(opcode: u8, s: io.Reader) -> (inst: Instruction, err: Decode_Instruction_Error) {
    size := Instruction_Size(opcode & 0b1)
    displacement, byte := instruction_displacement_from_stream(size, s) or_return
    sized_data := sized_instruction_data_from_stream(size, s) or_return

    i := Imm_To_Reg_Mem{sized_data = sized_data, displacement = displacement}

    switch (opcode >> 3) & 0b111 {
    case 0b000:
        inst = Add_Instruction(Add_Imm_To_Reg_Mem(i))
    case 0b101:
        inst = Sub_Instruction(Sub_Imm_To_Reg_Mem(i))
    case 0b111:
        inst = Cmp_Instruction(Cmp_Imm_To_Reg_Mem(i))
    case 0o306, 0o307:
        inst = Mov_Instruction(Mov_Imm_To_Reg_Mem(i))
    }

    return inst, nil
}

// reg to mem/reg
Reg_To_Mem :: struct {
    direction:    Instruction_Direction,
    size:         Instruction_Size,
    reg:          Register,
    displacement: Instruction_Displacement,
}

@private
reg_to_mem_string :: proc(opcode: string, rtm: Reg_To_Mem, allocator := context.allocator) -> string {
    d, r := instruction_displacement_string(rtm.displacement, allocator = allocator), register_string(rtm.reg, allocator = allocator)
    defer { delete(d, allocator = allocator); delete(r, allocator = allocator) }
    switch rtm.direction {
    case .Reg_Source:
        return fmt.aprintf("%s %s, %s", opcode, d, r, allocator = allocator)
    case .Reg_Destination:
        return fmt.aprintf("%s %s, %s", opcode, r, d, allocator = allocator)
    case: unreachable()
    }
}

@private
reg_to_mem :: proc(opcode: u8, s: io.Reader) -> (i: Reg_To_Mem, err: Decode_Instruction_Error) {
    direction := Instruction_Direction((opcode >> 1) & 0b1)
    size := Instruction_Size(opcode & 0b1)
    displacement, read_byte := instruction_displacement_from_stream(size, s) or_return
    reg := register_from_byte_with_size((read_byte >> 3) & 0b111, size)
    return Reg_To_Mem{
        direction = direction,
        size = size,
        displacement = displacement,
        reg = reg,
    }, nil
}

Mov_Reg_To_Mem :: Reg_To_Mem

Mov_Imm_To_Reg_Mem :: Imm_To_Reg_Mem

Mov_Imm_To_Reg :: struct {
    sized_data: Sized_Instruction_Data,
    reg:        Register,
}


Mov_Mem_To_Acc :: distinct Acc_Mem
Mov_Acc_To_Mem :: distinct Acc_Mem

Mov_Instruction :: union #no_nil { Mov_Reg_To_Mem, Mov_Imm_To_Reg_Mem, Mov_Imm_To_Reg, Mov_Mem_To_Acc, Mov_Acc_To_Mem }

Add_Reg_Mem_With_Register :: Reg_To_Mem
Add_Imm_To_Reg_Mem :: Imm_To_Reg_Mem
Add_Imm_To_Acc :: Imm_To_Acc

Add_Instruction :: union #no_nil { Add_Reg_Mem_With_Register, Add_Imm_To_Reg_Mem, Add_Imm_To_Acc }

Sub_Reg_Mem_With_Register :: Reg_To_Mem
Sub_Imm_To_Reg_Mem :: Imm_To_Reg_Mem
Sub_Imm_To_Acc :: Imm_To_Acc

Sub_Instruction :: union #no_nil { Sub_Reg_Mem_With_Register, Sub_Imm_To_Reg_Mem, Sub_Imm_To_Acc }

Cmp_Reg_Mem_With_Register :: Reg_To_Mem
Cmp_Imm_To_Reg_Mem :: Imm_To_Reg_Mem
Cmp_Imm_To_Acc :: Imm_To_Acc

Cmp_Instruction :: union #no_nil { Cmp_Reg_Mem_With_Register, Cmp_Imm_To_Reg_Mem, Cmp_Imm_To_Acc }

Instruction :: union { Mov_Instruction, Add_Instruction, Sub_Instruction, Cmp_Instruction }

instruction_string :: proc(i: Instruction, allocator := context.allocator) -> string {
    switch i in i {
    case Mov_Instruction:
        switch mov in i {
        case Mov_Reg_To_Mem:
            return reg_to_mem_string("mov", mov, allocator)
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
    case Add_Instruction:
        switch add in i {
        case Add_Reg_Mem_With_Register:
            return reg_to_mem_string("add", add, allocator)
        case Add_Imm_To_Reg_Mem:
            return imm_to_reg_mem_string("add", add, allocator)
        case Add_Imm_To_Acc:
            return imm_to_acc_string("add", add, allocator)
        }
    case Sub_Instruction:
        switch sub in i {
        case Sub_Reg_Mem_With_Register:
            return reg_to_mem_string("sub", sub, allocator)
        case Sub_Imm_To_Reg_Mem:
            return imm_to_reg_mem_string("sub", sub, allocator)
        case Sub_Imm_To_Acc:
            return imm_to_acc_string("sub", sub, allocator)
        }
    case Cmp_Instruction:
        switch cmp in i {
        case Cmp_Reg_Mem_With_Register:
            return reg_to_mem_string("cmp", cmp, allocator)
        case Cmp_Imm_To_Reg_Mem:
            return imm_to_reg_mem_string("cmp", cmp, allocator)
        case Cmp_Imm_To_Acc:
            return imm_to_acc_string("cmp", cmp, allocator)
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
    // Mov
    case 0o210..=0o213: // reg/men to/from reg
        return Mov_Instruction(reg_to_mem(opcode, s) or_return), nil
    case 0o306, 0o307: // imm to mem
        return imm_to_reg_mem(opcode, s) or_return, nil
    case 0o260..=0o277: // imm to reg
        size := Instruction_Size((opcode >> 3) & 0b1)
        reg := register_from_byte_with_size(opcode & 0b111, size)
        sized_data := sized_instruction_data_from_stream(size, s) or_return
        return Mov_Instruction(Mov_Imm_To_Reg{reg = reg, sized_data = sized_data}), nil
    case 0o240, 0o241:  // mem to acc
        size := Instruction_Size(opcode & 0b1)
        buffer: [2]u8
        _, read_err := io.read(s, buffer[:])
        if read_err != nil {
            err = .Missing_Data
            return
        }
        return Mov_Instruction(Mov_Mem_To_Acc{size = size, addr = transmute(u16)buffer}), nil
    case 0o242, 0o243: // acc to mem
        size := Instruction_Size(opcode & 0b1)
        buffer: [2]u8
        _, read_err := io.read(s, buffer[:])
        if read_err != nil {
            err = .Missing_Data
            return
        }
        return Mov_Instruction(Mov_Acc_To_Mem{size = size, addr = transmute(u16)buffer}), nil

    // Add
    case 0o000..=0o003:
        return Add_Instruction(reg_to_mem(opcode, s) or_return), nil
    case 0o004..=0o007:
        return Add_Instruction(imm_to_acc(opcode, s) or_return), nil

    // Sub
    case 0o050..=0o053:
        return Sub_Instruction(reg_to_mem(opcode, s) or_return), nil
    case 0o054..=0o057:
        return Sub_Instruction(imm_to_acc(opcode, s) or_return), nil

    // Cmp
    case 0o070..=0o073:
        return Cmp_Instruction(reg_to_mem(opcode, s) or_return), nil
    case 0o074..=0o077:
        return Cmp_Instruction(imm_to_acc(opcode, s) or_return), nil

    // Add, Sub, Cmp
    case 0o200..=0o203:
        return imm_to_reg_mem(opcode, s)
    }

    err = .Unsupported_Opcode
    return
}
