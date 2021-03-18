@import Cocoa ;
@import LuaSkin ;
@import Darwin.sys.sysctl ;

// Code primarily influenced by:
//  * https://opensource.apple.com/source/system_cmds/system_cmds-880.60.2/sysctl.tproj/ and

// In case I forget and try to use this later as a jumping off point for a linux or
// freebsd lua library
#ifndef __APPLE__
#error Tailored to work specifically with Apple sysctl implementation
#endif

static const char * const USERDATA_TAG = "hs.host.sysctl" ;
static LSRefTable refTable = LUA_NOREF;

// Shims for FreeBSD source compatibility.
#define CTLTYPE_UINT 0xa
#define CTLTYPE_LONG 0xb
#define CTLTYPE_ULONG 0xc
#define CTLTYPE_S64 0xd
#define CTLTYPE_U64 0xe

// #define CTLFLAG_TUN 0

// Support for CTL_USER
static const struct ctlname names[] = CTL_NAMES;
static const struct ctlname user_names[] = CTL_USER_NAMES;
static const int user_names_count = sizeof(user_names) / sizeof(*user_names);

static int ctl_size[CTLTYPE+1] = {
    [CTLTYPE_INT] = sizeof(int),
    [CTLTYPE_UINT] = sizeof(u_int),
    [CTLTYPE_LONG] = sizeof(long),
    [CTLTYPE_ULONG] = sizeof(u_long),
    [CTLTYPE_S64] = sizeof(int64_t),
    [CTLTYPE_U64] = sizeof(int64_t),
};

#pragma mark - Module Functions

// we *could* reduce this file down to just the sysctl function (and compile time defined
// variables) by using sysctl.name2oid but that is discouraged because it's considered
// horrible code and *may* (it hasn't in over a decade, though) disappear at some point.
// Using sysctlnametomib should be a little more future-proof.
//
// FWIW, in FreeBSD (I couldn't find it in Apple's opensource stuff in the "obvious"
// places, and they don't give a useful search tool) sysctlnametomib *is* in fact just
// a wrapper that uses the sysctl.name2oid internal directly.
static int sysctl_sysctlnametomib(lua_State *L) {
    LuaSkin *skin = [LuaSkin sharedWithState:L] ;
    [skin checkArgs:LS_TSTRING, LS_TBREAK] ;
    const char *name = lua_tostring(L, 1) ;

    int    oidp[CTL_MAXNAME] ;
    size_t sizep = CTL_MAXNAME ;

    if (sysctlnametomib(name, oidp, &sizep)) {
        lua_pushnil(L) ;
        lua_pushinteger(L, errno) ;
        return 2 ;
    }

    lua_newtable(L) ;
    for (unsigned int i = 0 ; i < sizep ; i++) {
        lua_pushinteger(L, oidp[i]) ;
        lua_rawseti(L, -2, luaL_len(L, -2) + 1) ;
    }

    return 1 ;
}

static int sysctl_sysctl(lua_State *L) {
    LuaSkin *skin = [LuaSkin sharedWithState:L] ;
    [skin checkArgs:LS_TTABLE, LS_TSTRING | LS_TOPTIONAL, LS_TBREAK] ;
    NSArray *NSoid  = [skin toNSObjectAtIndex:1] ;
    NSData  *newVal = (lua_gettop(L) > 1) ? [skin toNSObjectAtIndex:2 withOptions:LS_NSLuaStringAsDataOnly] : nil ;

    if (![NSoid isKindOfClass:[NSArray class]]) {
        return luaL_error(L, "expected table containing integers") ;
    }
    unsigned int len  = (unsigned int)NSoid.count ;
    int          *oid = malloc(len * sizeof(int)) ;
    if (!oid) {
        lua_pushnil(L) ;
        lua_pushinteger(L, errno) ;
        return 2 ;
    }

    for (unsigned int i = 0 ; i < len ; i++) {
        NSNumber *entry = NSoid[i] ;
        if (![entry isKindOfClass:[NSNumber class]]) {
            return luaL_error(L, "expected table containing integers") ;
        }
        oid[i] = entry.intValue ;
    }

    BOOL sysctl_name2oid = (len == 2) && (oid[0] == 0) && (oid[1] == 3) ;

    if (!sysctl_name2oid && newVal) {
        if (sysctl(oid, len, NULL, 0, (void *)(uintptr_t)newVal.bytes, newVal.length)) {
            lua_pushnil(L) ;
            lua_pushinteger(L, errno) ;
            return 2 ;
        }
    }

    size_t rLen ;
    if (!sysctl_name2oid) {
        if (sysctl(oid, len, NULL, &rLen, NULL, 0)) {
            lua_pushnil(L) ;
            lua_pushinteger(L, errno) ;
            return 2 ;
        }
    } else {
        rLen = CTL_MAXNAME * sizeof(int) ;
    }

    // doubling *and* adding one seems overkill, but they do it in the sysctl source,
    // so we will here as well
    rLen += rLen ;
    u_char *result = malloc(rLen + 1);
    if (result == NULL) {
        lua_pushnil(L) ;
        lua_pushinteger(L, errno) ;
        return 2 ;
    }
    bzero(result, rLen + 1) ;

    if (!sysctl_name2oid) {
        if (sysctl(oid, len, result, &rLen, NULL, 0)) {
            lua_pushnil(L) ;
            lua_pushinteger(L, errno) ;
            return 2 ;
        }
    } else {
        if (sysctl(oid, len, result, &rLen, (void *)(uintptr_t)newVal.bytes, newVal.length)) {
            lua_pushnil(L) ;
            lua_pushinteger(L, errno) ;
            return 2 ;
        }
    }

    [skin pushNSObject:[NSData dataWithBytesNoCopy:result length:rLen] ] ;
    return 1 ;
}

#pragma mark - Module Constants

static int sysctl_types(lua_State *L) {
    lua_newtable(L) ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE) ;         lua_setfield(L, -2, "typeMask") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_NODE) ;    lua_setfield(L, -2, "node") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_INT) ;     lua_setfield(L, -2, "int") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_STRING) ;  lua_setfield(L, -2, "string") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_QUAD) ;    lua_setfield(L, -2, "quad") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_OPAQUE) ;  lua_setfield(L, -2, "opaque") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_STRUCT) ;  lua_setfield(L, -2, "struct") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_UINT) ;    lua_setfield(L, -2, "unsignedInt") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_LONG) ;    lua_setfield(L, -2, "long") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_ULONG) ;   lua_setfield(L, -2, "unsignedLong") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_S64) ;     lua_setfield(L, -2, "signed64") ;
    lua_pushinteger(L, (lua_Integer)CTLTYPE_U64) ;     lua_setfield(L, -2, "unsigned64") ;
    return 1 ;
}

static int sysctl_typeSizes(lua_State *L) {
    LuaSkin *skin = [LuaSkin sharedWithState:L] ;
    lua_newtable(L) ;
    [skin pushNSObject:[NSString stringWithFormat:@"i%d", ctl_size[CTLTYPE_INT]]] ;   lua_setfield(L, -2, "int") ;
    [skin pushNSObject:[NSString stringWithFormat:@"I%d", ctl_size[CTLTYPE_UINT]]] ;  lua_setfield(L, -2, "unsignedInt") ;
    [skin pushNSObject:[NSString stringWithFormat:@"i%d", ctl_size[CTLTYPE_LONG]]] ;  lua_setfield(L, -2, "long") ;
    [skin pushNSObject:[NSString stringWithFormat:@"I%d", ctl_size[CTLTYPE_ULONG]]] ; lua_setfield(L, -2, "unsignedLong") ;
    [skin pushNSObject:[NSString stringWithFormat:@"i%d", ctl_size[CTLTYPE_S64]]] ;   lua_setfield(L, -2, "signed64") ;
    [skin pushNSObject:[NSString stringWithFormat:@"I%d", ctl_size[CTLTYPE_U64]]] ;   lua_setfield(L, -2, "unsigned64") ;
    [skin pushNSObject:[NSString stringWithFormat:@"i%lu", sizeof(int64_t)]] ;        lua_setfield(L, -2, "Q") ;
    [skin pushNSObject:[NSString stringWithFormat:@"I%lu", sizeof(uint64_t)]] ;       lua_setfield(L, -2, "QU") ;

    [skin pushNSObject:[NSString stringWithFormat:@"i%lu", sizeof(int8_t)]] ;    lua_setfield(L, -2, "int8_t") ;
    [skin pushNSObject:[NSString stringWithFormat:@"I%lu", sizeof(u_int8_t)]] ;  lua_setfield(L, -2, "u_int8_t") ;
    [skin pushNSObject:[NSString stringWithFormat:@"i%lu", sizeof(int16_t)]] ;   lua_setfield(L, -2, "int16_t") ;
    [skin pushNSObject:[NSString stringWithFormat:@"I%lu", sizeof(u_int16_t)]] ; lua_setfield(L, -2, "u_int16_t") ;
    [skin pushNSObject:[NSString stringWithFormat:@"i%lu", sizeof(int32_t)]] ;   lua_setfield(L, -2, "int32_t") ;
    [skin pushNSObject:[NSString stringWithFormat:@"I%lu", sizeof(u_int32_t)]] ; lua_setfield(L, -2, "u_int32_t") ;
    [skin pushNSObject:[NSString stringWithFormat:@"i%lu", sizeof(int64_t)]] ;   lua_setfield(L, -2, "int64_t") ;
    [skin pushNSObject:[NSString stringWithFormat:@"I%lu", sizeof(u_int64_t)]] ; lua_setfield(L, -2, "u_int64_t") ;

    [skin pushNSObject:[NSString stringWithFormat:@"i%lu", sizeof(short)]] ;     lua_setfield(L, -2, "short") ;
    [skin pushNSObject:[NSString stringWithFormat:@"I%lu", sizeof(u_short)]] ;   lua_setfield(L, -2, "unsignedShort") ;

    [skin pushNSObject:[NSString stringWithFormat:@"I%lu", sizeof(fixpt_t)]] ;   lua_setfield(L, -2, "fixpt_t") ;
    [skin pushNSObject:[NSString stringWithFormat:@"i%lu", sizeof(boolean_t)]] ; lua_setfield(L, -2, "boolean_t") ;
    [skin pushNSObject:@"b"] ;                                                   lua_setfield(L, -2, "char") ;
    [skin pushNSObject:@"B"] ;                                                   lua_setfield(L, -2, "unsignedChar") ;
    [skin pushNSObject:@"d"] ;                                                   lua_setfield(L, -2, "double") ;
    [skin pushNSObject:@"f"] ;                                                   lua_setfield(L, -2, "float") ;

    return 1 ;
}

static int sysctl_flags(lua_State *L) {
    lua_newtable(L) ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_RD) ;      lua_setfield(L, -2, "read") ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_WR) ;      lua_setfield(L, -2, "write") ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_RW) ;      lua_setfield(L, -2, "readWrite") ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_NOLOCK) ;  lua_setfield(L, -2, "noLock") ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_ANYBODY) ; lua_setfield(L, -2, "unsecuredWrite") ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_SECURE) ;  lua_setfield(L, -2, "securedWrite") ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_MASKED) ;  lua_setfield(L, -2, "masked") ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_NOAUTO) ;  lua_setfield(L, -2, "noAutoRegister") ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_KERN) ;    lua_setfield(L, -2, "kernel") ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_LOCKED) ;  lua_setfield(L, -2, "nodeLocks") ;
    lua_pushinteger(L, (lua_Integer)CTLFLAG_OID2) ;    lua_setfield(L, -2, "hasOIDVersionInfo") ;
    return 1 ;
}

static int sysctl_userFields(lua_State *L) {
    lua_newtable(L) ;
    for (int i = 1; i < user_names_count; ++i) {
        lua_newtable(L) ;
        lua_pushstring(L, user_names[i].ctl_name) ;  lua_rawseti(L, -2, luaL_len(L, -2) + 1) ;
        lua_pushinteger(L, user_names[i].ctl_type) ; lua_rawseti(L, -2, luaL_len(L, -2) + 1) ;
        lua_rawseti(L, -2, luaL_len(L, -2) + 1) ;
    }
    lua_newtable(L) ;
    lua_pushstring(L, names[CTL_USER].ctl_name) ; lua_rawseti(L, -2, luaL_len(L, -2) + 1) ;
    lua_pushinteger(L, CTL_USER) ;                lua_rawseti(L, -2, luaL_len(L, -2) + 1) ;
    lua_rawseti(L, -2, 0) ;

    return 1 ;
}

#pragma mark - Hammerspoon/Lua Infrastructure

// Functions for returned object when module loads
static luaL_Reg moduleLib[] = {
    {"oidFromName", sysctl_sysctlnametomib},
    {"sysctl",    sysctl_sysctl},
    {NULL, NULL}
};

int luaopen_hs_host_sysctl_internal(lua_State* L) {
    LuaSkin *skin = [LuaSkin sharedWithState:L] ;
    refTable = [skin registerLibrary:USERDATA_TAG functions:moduleLib metaFunctions:nil] ;

    sysctl_types(L) ;      lua_setfield(L, -2, "types") ;
    sysctl_typeSizes(L) ;  lua_setfield(L, -2, "typeSizes") ;
    sysctl_flags(L) ;      lua_setfield(L, -2, "flags") ;

    // pass some compile time constants that are needed lua side
    sysctl_userFields(L) ;            lua_setfield(L, -2, "userFields") ;

    return 1;
}
