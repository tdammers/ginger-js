function asText(a) {
    switch (typeof(a)) {
        case 'string': return a
        case 'number': return '' + a
        case 'object':
            if (Array.isArray(a)) {
                return a.map(asText).join()
            }
            else {
                var keys = Object.getOwnPropertyNames(a)
                return keys.map(function(key) {
                    return key + ": " + asText(a[key])
                }).join()
            }
        default: return ''
    }
}

function asListItems(a) {
    switch (typeof(a)) {
        case 'object':
            if (Array.isArray(a)) {
                return a
            }
            else {
                var keys = Object.getOwnPropertyNames(a)
                return keys.map(function(key) {
                    return a[key]
                })
            }
        default:
            return null
    }
}

function asDictItems(a) {
    switch (typeof(a)) {
        case 'object':
            if (Array.isArray(a)) {
                return null
            }
            else {
                var keys = Object.getOwnPropertyNames(a)
                return keys.map(function(key) {
                    return [key, a[key]]
                })
            }
        default:
            return null
    }
}

function fromDictItems(items) {
    var o = {}
    var pair, key, val
    var i

    if (!Array.isArray(items)) return {}

    for (i = 0; i < items.length; ++i) {
        pair = items[i]
        key = pair[0] + ''
        val = pair[1] || null
        if (key == '') continue // skip empty keys
        o[key] = val
    }

    return o
}

function fromListItems(items) {
    if (!Array.isArray(items)) return []
    return items
}

function throwError(msg) {
    throw msg;
}
