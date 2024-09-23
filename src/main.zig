const std = @import("std");
const Wyhash = std.hash.Wyhash;

fn TakeOut(comptime T: type, comptime fields: []const []const u8) type {
    const info = @typeInfo(T).Struct;

    var keep: []const std.builtin.Type.StructField = &.{};

    for (info.fields) |field| {
        for (fields) |remove_field| {
            if (std.mem.eql(u8, field.name, remove_field)) {
                break;
            }
        } else {
            keep = keep ++ .{field};
        }
    }

    return @Type(.{
        .Struct = .{
            .layout = .auto,
            .backing_integer = null,
            .fields = keep,
            .decls = &.{},
            .is_tuple = info.is_tuple,
        },
    });
}

fn takeOut(value: anytype, comptime fields: []const []const u8) TakeOut(@TypeOf(value), fields) {
    const New = TakeOut(@TypeOf(value), fields);

    var result: New = undefined;
    inline for (@typeInfo(New).Struct.fields) |field| {
        @field(result, field.name) = @field(value, field.name);
    }

    return result;
}

const Hasher = struct {
    const Self = @This();

    inner: Wyhash = Wyhash.init(0),
    present_types: []const type = &.{},

    fn update(self: *Hasher, input: []const u8) void {
        self.inner.update(input);
    }

    fn hashDecls(self: *Hasher, T: type, decls: []const std.builtin.Type.Declaration) void {
        for (decls) |decl| {
            self.hash(decl.name);
            const value = @field(T, decl.name);
            const Value = @TypeOf(value);
            if (@typeInfo(Value) == .Fn) {
                self.hash(Value);
            } else {
                self.hash(value);
            }
        }
    }

    fn hashType(self: *Hasher, T: type) void {
        for (self.present_types, 0..) |O, i| {
            if (T == O) {
                self.hash(.{ @as(u8, 255), @as(u64, i) });
                return;
            }
        }
        self.present_types = self.present_types ++ .{T};

        self.hash(@typeInfo(T));

        const type_info = @typeInfo(T);
        self.hash(@as(u8, @intFromEnum(type_info)));
        switch (type_info) {
            // Types with no additional info
            .Type, .Void, .Bool, .NoReturn, .ComptimeFloat, .ComptimeInt, .Undefined, .Null, .EnumLiteral => {},
            .Pointer => |info| {
                self.hash(takeOut(info, &.{"sentinel"}));
                if (info.sentinel) |value| {
                    self.hash(@as(*const info.child, @alignCast(@ptrCast(value))).*);
                }
            },
            .Array => |info| {
                self.hash(takeOut(info, &.{"sentinel"}));
                if (info.sentinel) |value| {
                    self.hash(@as(*const info.child, @alignCast(@ptrCast(value))).*);
                }
            },
            .Struct => |info| {
                self.hash(takeOut(info, &.{ "fields", "decls" }));
                for (info.fields) |field| {
                    self.hash(takeOut(field, &.{"default_value"}));
                    if (field.default_value) |value| {
                        self.hash(@as(*const field.type, @alignCast(@ptrCast(value))).*);
                    }
                }
                self.hashDecls(T, info.decls);
            },
            .Enum => |info| {
                self.hash(takeOut(info, &.{"decls"}));
                self.hashDecls(T, info.decls);
            },
            .Union => |info| {
                self.hash(takeOut(info, &.{"decls"}));
                self.hashDecls(T, info.decls);
            },
            .Opaque => |info| {
                self.hash(takeOut(info, &.{"decls"}));
                self.hashDecls(T, info.decls);
            },
            else => |other| self.hash(other),
        }
    }

    pub fn hash(self: *Hasher, value: anytype) void {
        const type_info = @typeInfo(@TypeOf(value));

        switch (type_info) {
            .Void, .NoReturn, .Undefined, .Null, .Opaque, .Fn, .Frame, .AnyFrame, .EnumLiteral => {},
            .Type => self.hashType(value),
            .Bool, .Int, .Float => self.update(&std.mem.toBytes(value)),
            .Array, .Vector => |arr| {
                self.hash(arr.child);
                for (value) |inner| {
                    self.hash(inner);
                }
            },
            .ComptimeFloat => self.hash(@as(f128, value)),
            .ComptimeInt => self.hash(@as(i128, value)),
            .Pointer => |ptr| {
                self.hash(ptr.child);
                switch (ptr.size) {
                    .One => {
                        if (ptr.child != anyopaque) {
                            self.hash(value.*);
                        }
                    },
                    .Slice => {
                        self.hash(@as(u64, value.len));
                        if (ptr.child != anyopaque) {
                            for (value) |inner| {
                                self.hash(inner);
                            }
                        }
                    },
                    else => @compileError(
                        "Cannot get type id because type `" ++
                            @typeName(@TypeOf(value)) ++
                            "` doesn't have a known size",
                    ),
                }
            },
            .Struct => |st| {
                inline for (st.fields) |field| {
                    self.hash(@field(value, field.name));
                }
            },
            .Optional => if (value) |inner| self.hash(inner),

            .ErrorUnion => blk: {
                const payload = value catch |err| {
                    self.hash(err);
                    break :blk;
                };
                self.hash(payload);
            },
            .ErrorSet => self.hash(@intFromError(value)),
            .Enum => self.hash(@intFromEnum(value)),
            .Union => |un| {
                if (un.tag_type) |tag_type| {
                    const tag = std.meta.activeTag(value);
                    self.hash(tag);
                    inline for (un.fields) |field| {
                        if (tag == @field(tag_type, field.name)) {
                            self.hash(@field(value, field.name));
                            return;
                        }
                    }
                    unreachable;
                } else {
                    @compileError(
                        "Cannot get type id because type `" ++
                            @typeName(@TypeOf(value)) ++
                            "` doesn't have a known size",
                    );
                }
            },
        }
    }

    pub fn final(self: *Hasher) u64 {
        return self.inner.final();
    }
};

fn rerpId(comptime T: type) u64 {
    return comptime result: {
        @setEvalBranchQuota(100_000);
        var hasher = Hasher{};
        hasher.hash(T);
        break :result hasher.final();
    };
}

test {
    _ = rerpId(type);
    _ = rerpId(void);
    _ = rerpId(bool);
    _ = rerpId(noreturn);
    _ = rerpId(u16);
    _ = rerpId(f16);
    _ = rerpId(*u8);
    _ = rerpId([*]u8);
    _ = rerpId([*:0]u8);
    _ = rerpId([]u8);
    _ = rerpId([:0]u8);
    _ = rerpId([*c]u8);
    _ = rerpId([64]u8);
    _ = rerpId([64:0]u8);
    _ = rerpId(struct { u16 });
    _ = rerpId(struct { value: u16 });
    _ = rerpId(struct { value: u16 = 16 });
    _ = rerpId(struct { value: *const anyopaque = @ptrFromInt(13) });
    _ = rerpId(packed struct { value: u16 });
    _ = rerpId(comptime_float);
    _ = rerpId(comptime_int);
    _ = rerpId(@TypeOf(undefined));
    _ = rerpId(@TypeOf(null));
    _ = rerpId(?u8);
    _ = rerpId(error{OutOfMemory}!void);
    _ = rerpId(error{OutOfMemory});
    _ = rerpId(enum { some, other });
    _ = rerpId(union { some: u8, other: u16 });
    _ = rerpId(union(enum) { some: u8, other: u16 });
    const e = enum { some, other };
    _ = rerpId(union(e) { some: u8, other: u16 });
    _ = rerpId(fn () anyerror!void);
    _ = rerpId(opaque {
        const Self = @This();
        fn action(self: *Self) void {
            _ = self;
        }
    });
    _ = rerpId(@Vector(3, f32));
    _ = rerpId(@TypeOf(.something));
}

const Test1 = struct {
    inner: u64,

    pub fn testFunction() void {}
};

const Test2 = struct {
    inner: u64,

    pub fn testFunction() void {}
};

pub fn main() !void {
    std.debug.print("{}\n", .{Test1 == Test2}); // false
    std.debug.print("{}\n", .{rerpId(Test1)}); // 1152012182388405333
    std.debug.print("{}\n", .{rerpId(Test2)}); // 1152012182388405333
}
