# `.applyLogTransformation` correctly log-transforms yValues and lloq

    WAoAAAACAAQDAgACAwAAAAMTAAAAGAAAAA4AAAAXAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAf/AAAAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/8AAA
    AAAHon/wAAAAAAeif/AAAAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/8AAAAAAHogAA
    AA4AAAAXAAAAAAAAAABAMF0XX+38R0A9F0XgCRSnQEa6Lp/7iHdAUUXRQAPRG0BYF0ZAAEsp
    QF2Lov/fSNBAZoAAAAAAAEBy3RdACC3MQHpAAAAAAABAhJF0v/stMkCR10Wf9P1tQDBdF1/t
    /EdAPRdF4AkUp0BGui6f+4h3QFFF0UAD0RtAWBdGQABLKUBdi6L/30jQQGaAAAAAAABAct0X
    QAgtzEB6QAAAAAAAQISRdL/7LTJAkddFn/T9bQAAABAAAAAXAAQACQAAACQwMGE1MTAyNS0w
    Mzk5LTRkODAtODUzZC0yMWIzZjVmM2FkM2MABAAJAAAAJDAwYTUxMDI1LTAzOTktNGQ4MC04
    NTNkLTIxYjNmNWYzYWQzYwAEAAkAAAAkMDBhNTEwMjUtMDM5OS00ZDgwLTg1M2QtMjFiM2Y1
    ZjNhZDNjAAQACQAAACQwMGE1MTAyNS0wMzk5LTRkODAtODUzZC0yMWIzZjVmM2FkM2MABAAJ
    AAAAJDAwYTUxMDI1LTAzOTktNGQ4MC04NTNkLTIxYjNmNWYzYWQzYwAEAAkAAAAkMDBhNTEw
    MjUtMDM5OS00ZDgwLTg1M2QtMjFiM2Y1ZjNhZDNjAAQACQAAACQwMGE1MTAyNS0wMzk5LTRk
    ODAtODUzZC0yMWIzZjVmM2FkM2MABAAJAAAAJDAwYTUxMDI1LTAzOTktNGQ4MC04NTNkLTIx
    YjNmNWYzYWQzYwAEAAkAAAAkMDBhNTEwMjUtMDM5OS00ZDgwLTg1M2QtMjFiM2Y1ZjNhZDNj
    AAQACQAAACQwMGE1MTAyNS0wMzk5LTRkODAtODUzZC0yMWIzZjVmM2FkM2MABAAJAAAAJDAw
    YTUxMDI1LTAzOTktNGQ4MC04NTNkLTIxYjNmNWYzYWQzYwAEAAkAAAAkMDBhNTEwMjUtMDM5
    OS00ZDgwLTg1M2QtMjFiM2Y1ZjNhZDNjAAQACQAAACdBY2ljbG92aXJMYXNraW5EYXRhLkxh
    c2tpbiAxOTgyLkdyb3VwIEEABAAJAAAAJ0FjaWNsb3Zpckxhc2tpbkRhdGEuTGFza2luIDE5
    ODIuR3JvdXAgQQAEAAkAAAAnQWNpY2xvdmlyTGFza2luRGF0YS5MYXNraW4gMTk4Mi5Hcm91
    cCBBAAQACQAAACdBY2ljbG92aXJMYXNraW5EYXRhLkxhc2tpbiAxOTgyLkdyb3VwIEEABAAJ
    AAAAJ0FjaWNsb3Zpckxhc2tpbkRhdGEuTGFza2luIDE5ODIuR3JvdXAgQQAEAAkAAAAnQWNp
    Y2xvdmlyTGFza2luRGF0YS5MYXNraW4gMTk4Mi5Hcm91cCBBAAQACQAAACdBY2ljbG92aXJM
    YXNraW5EYXRhLkxhc2tpbiAxOTgyLkdyb3VwIEEABAAJAAAAJ0FjaWNsb3Zpckxhc2tpbkRh
    dGEuTGFza2luIDE5ODIuR3JvdXAgQQAEAAkAAAAnQWNpY2xvdmlyTGFza2luRGF0YS5MYXNr
    aW4gMTk4Mi5Hcm91cCBBAAQACQAAACdBY2ljbG92aXJMYXNraW5EYXRhLkxhc2tpbiAxOTgy
    Lkdyb3VwIEEABAAJAAAAJ0FjaWNsb3Zpckxhc2tpbkRhdGEuTGFza2luIDE5ODIuR3JvdXAg
    QQAAAA4AAAAXwEcGniqiqltADJ211E+mnEAHekb0CuA0QAUDV8k9Ga9AA83AMELbO0ACxYJo
    qmsSQAH4OahJCOM//3E7Cup9Ez/2r+KX3LceP+yXZRmj+W6/wo0u4UrBiMABZ+Vf+xC5QALU
    dJ8RRsdABJIIikMZzkAE6yYcrHlkQATrJhyseWRAAiI5aWz+4kABNJU41syVP/wlFl+mE1A/
    8tys26CUtz/lA87uLnscv9QqWd3soRTAASQk8YendwAAABAAAAAXAAQACQAAAARUaW1lAAQA
    CQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARU
    aW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQA
    CQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARU
    aW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQA
    CQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAQACQAAAARUaW1lAAAAEAAAABcA
    BAAJAAAAA21pbgAEAAkAAAADbWluAAQACQAAAANtaW4ABAAJAAAAA21pbgAEAAkAAAADbWlu
    AAQACQAAAANtaW4ABAAJAAAAA21pbgAEAAkAAAADbWluAAQACQAAAANtaW4ABAAJAAAAA21p
    bgAEAAkAAAADbWluAAQACQAAAANtaW4ABAAJAAAAA21pbgAEAAkAAAADbWluAAQACQAAAANt
    aW4ABAAJAAAAA21pbgAEAAkAAAADbWluAAQACQAAAANtaW4ABAAJAAAAA21pbgAEAAkAAAAD
    bWluAAQACQAAAANtaW4ABAAJAAAAA21pbgAEAAkAAAADbWluAAAAEAAAABcABAAJAAAAFUNv
    bmNlbnRyYXRpb24gKG1vbGFyKQAEAAkAAAAVQ29uY2VudHJhdGlvbiAobW9sYXIpAAQACQAA
    ABVDb25jZW50cmF0aW9uIChtb2xhcikABAAJAAAAFUNvbmNlbnRyYXRpb24gKG1vbGFyKQAE
    AAkAAAAVQ29uY2VudHJhdGlvbiAobW9sYXIpAAQACQAAABVDb25jZW50cmF0aW9uIChtb2xh
    cikABAAJAAAAFUNvbmNlbnRyYXRpb24gKG1vbGFyKQAEAAkAAAAVQ29uY2VudHJhdGlvbiAo
    bW9sYXIpAAQACQAAABVDb25jZW50cmF0aW9uIChtb2xhcikABAAJAAAAFUNvbmNlbnRyYXRp
    b24gKG1vbGFyKQAEAAkAAAAVQ29uY2VudHJhdGlvbiAobW9sYXIpAAQACQAAABVDb25jZW50
    cmF0aW9uIChtb2xhcikABAAJAAAAFENvbmNlbnRyYXRpb24gKG1hc3MpAAQACQAAABRDb25j
    ZW50cmF0aW9uIChtYXNzKQAEAAkAAAAUQ29uY2VudHJhdGlvbiAobWFzcykABAAJAAAAFENv
    bmNlbnRyYXRpb24gKG1hc3MpAAQACQAAABRDb25jZW50cmF0aW9uIChtYXNzKQAEAAkAAAAU
    Q29uY2VudHJhdGlvbiAobWFzcykABAAJAAAAFENvbmNlbnRyYXRpb24gKG1hc3MpAAQACQAA
    ABRDb25jZW50cmF0aW9uIChtYXNzKQAEAAkAAAAUQ29uY2VudHJhdGlvbiAobWFzcykABAAJ
    AAAAFENvbmNlbnRyYXRpb24gKG1hc3MpAAQACQAAABRDb25jZW50cmF0aW9uIChtYXNzKQAA
    ABAAAAAXAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACA
    CQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1v
    bC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAA
    AAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9s
    AACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfC
    tW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAACA
    CQAAAAfCtW1vbC9sAACACQAAAAfCtW1vbC9sAAAADgAAABdAbCa4UeuFH0BsJrhR64UfQGwm
    uFHrhR9AbCa4UeuFH0BsJrhR64UfQGwmuFHrhR9AbCa4UeuFH0BsJrhR64UfQGwmuFHrhR9A
    bCa4UeuFH0BsJrhR64UfQGwmuFHrhR9AbCa4UeuFH0BsJrhR64UfQGwmuFHrhR9AbCa4UeuF
    H0BsJrhR64UfQGwmuFHrhR9AbCa4UeuFH0BsJrhR64UfQGwmuFHrhR9AbCa4UeuFH0BsJrhR
    64UfAAAAEAAAABcABAAJAAAACXNpbXVsYXRlZAAEAAkAAAAJc2ltdWxhdGVkAAQACQAAAAlz
    aW11bGF0ZWQABAAJAAAACXNpbXVsYXRlZAAEAAkAAAAJc2ltdWxhdGVkAAQACQAAAAlzaW11
    bGF0ZWQABAAJAAAACXNpbXVsYXRlZAAEAAkAAAAJc2ltdWxhdGVkAAQACQAAAAlzaW11bGF0
    ZWQABAAJAAAACXNpbXVsYXRlZAAEAAkAAAAJc2ltdWxhdGVkAAQACQAAAAlzaW11bGF0ZWQA
    BAAJAAAACG9ic2VydmVkAAQACQAAAAhvYnNlcnZlZAAEAAkAAAAIb2JzZXJ2ZWQABAAJAAAA
    CG9ic2VydmVkAAQACQAAAAhvYnNlcnZlZAAEAAkAAAAIb2JzZXJ2ZWQABAAJAAAACG9ic2Vy
    dmVkAAQACQAAAAhvYnNlcnZlZAAEAAkAAAAIb2JzZXJ2ZWQABAAJAAAACG9ic2VydmVkAAQA
    CQAAAAhvYnNlcnZlZAAAAA4AAAAXf/AAAAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/
    8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/8AAAAAAH
    on/wAAAAAAeif/AAAAAAB6JAFco/MJd3gEAWwlVTu+E6QCAkZUDmPfx/8AAAAAAHon/wAAAA
    AAeif/AAAAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/8AAAAAAHogAAABAAAAAXAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQACQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVu
    b3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAoUGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAQA
    CQAAAElPcmdhbmlzbXxQZXJpcGhlcmFsVmVub3VzQmxvb2R8QWNpY2xvdmlyfFBsYXNtYSAo
    UGVyaXBoZXJhbCBWZW5vdXMgQmxvb2QpAAAAEAAAABcAAAAJ/////wAAAAn/////AAAACf//
    //8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ
    /////wAAAAn/////AAAACf////8ABAAJAAAAEEFyaXRobWV0aWNTdGREZXYABAAJAAAAEEFy
    aXRobWV0aWNTdGREZXYABAAJAAAAEEFyaXRobWV0aWNTdGREZXYABAAJAAAAEEFyaXRobWV0
    aWNTdGREZXYABAAJAAAAEEFyaXRobWV0aWNTdGREZXYABAAJAAAAEEFyaXRobWV0aWNTdGRE
    ZXYABAAJAAAAEEFyaXRobWV0aWNTdGREZXYABAAJAAAAEEFyaXRobWV0aWNTdGREZXYABAAJ
    AAAAEEFyaXRobWV0aWNTdGREZXYABAAJAAAAEEFyaXRobWV0aWNTdGREZXYABAAJAAAAEEFy
    aXRobWV0aWNTdGREZXYAAAAQAAAAFwAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAA
    gAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVt
    b2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkA
    AAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wv
    bAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAH
    wrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAA
    gAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAgAkAAAAHwrVtb2wvbAAAAA4AAAAXf/AA
    AAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/
    8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/8AAAAAAH
    on/wAAAAAAeif/AAAAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/8AAAAAAHon/wAAAA
    AAeif/AAAAAAB6J/8AAAAAAHogAAABAAAAAXAAAACf////8AAAAJ/////wAAAAn/////AAAA
    Cf////8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8A
    AAAJ/////wAAAAn/////AAQACQAAABNMYXNraW4gMTk4Mi5Hcm91cCBBAAQACQAAABNMYXNr
    aW4gMTk4Mi5Hcm91cCBBAAQACQAAABNMYXNraW4gMTk4Mi5Hcm91cCBBAAQACQAAABNMYXNr
    aW4gMTk4Mi5Hcm91cCBBAAQACQAAABNMYXNraW4gMTk4Mi5Hcm91cCBBAAQACQAAABNMYXNr
    aW4gMTk4Mi5Hcm91cCBBAAQACQAAABNMYXNraW4gMTk4Mi5Hcm91cCBBAAQACQAAABNMYXNr
    aW4gMTk4Mi5Hcm91cCBBAAQACQAAABNMYXNraW4gMTk4Mi5Hcm91cCBBAAQACQAAABNMYXNr
    aW4gMTk4Mi5Hcm91cCBBAAQACQAAABNMYXNraW4gMTk4Mi5Hcm91cCBBAAAAEAAAABcAAAAJ
    /////wAAAAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAA
    AAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8ABAAJAAAAE0xhc2tpbiAx
    OTgyLkdyb3VwIEEABAAJAAAAE0xhc2tpbiAxOTgyLkdyb3VwIEEABAAJAAAAE0xhc2tpbiAx
    OTgyLkdyb3VwIEEABAAJAAAAE0xhc2tpbiAxOTgyLkdyb3VwIEEABAAJAAAAE0xhc2tpbiAx
    OTgyLkdyb3VwIEEABAAJAAAAE0xhc2tpbiAxOTgyLkdyb3VwIEEABAAJAAAAE0xhc2tpbiAx
    OTgyLkdyb3VwIEEABAAJAAAAE0xhc2tpbiAxOTgyLkdyb3VwIEEABAAJAAAAE0xhc2tpbiAx
    OTgyLkdyb3VwIEEABAAJAAAAE0xhc2tpbiAxOTgyLkdyb3VwIEEABAAJAAAAE0xhc2tpbiAx
    OTgyLkdyb3VwIEEAAAAOAAAAF3/wAAAAAAeif/AAAAAAB6J/8AAAAAAHon/wAAAAAAeif/AA
    AAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/8AAAAAAHon/wAAAAAAeif/AAAAAAB6J/
    8AAAAAAHoj/wAAAAAAAAP/AAAAAAAAA/8AAAAAAAAD/wAAAAAAAAP/AAAAAAAAA/8AAAAAAA
    AD/wAAAAAAAAP/AAAAAAAAA/8AAAAAAAAD/wAAAAAAAAP/AAAAAAAAAAAAAQAAAAFwAAAAn/
    ////AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAA
    Cf////8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAEAAkAAAAVUGVyaXBoZXJh
    bFZlbm91c0Jsb29kAAQACQAAABVQZXJpcGhlcmFsVmVub3VzQmxvb2QABAAJAAAAFVBlcmlw
    aGVyYWxWZW5vdXNCbG9vZAAEAAkAAAAVUGVyaXBoZXJhbFZlbm91c0Jsb29kAAQACQAAABVQ
    ZXJpcGhlcmFsVmVub3VzQmxvb2QABAAJAAAAFVBlcmlwaGVyYWxWZW5vdXNCbG9vZAAEAAkA
    AAAVUGVyaXBoZXJhbFZlbm91c0Jsb29kAAQACQAAABVQZXJpcGhlcmFsVmVub3VzQmxvb2QA
    BAAJAAAAFVBlcmlwaGVyYWxWZW5vdXNCbG9vZAAEAAkAAAAVUGVyaXBoZXJhbFZlbm91c0Js
    b29kAAQACQAAABVQZXJpcGhlcmFsVmVub3VzQmxvb2QAAAAQAAAAFwAAAAn/////AAAACf//
    //8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ
    /////wAAAAn/////AAAACf////8AAAAJ/////wAEAAkAAAAGUGxhc21hAAQACQAAAAZQbGFz
    bWEABAAJAAAABlBsYXNtYQAEAAkAAAAGUGxhc21hAAQACQAAAAZQbGFzbWEABAAJAAAABlBs
    YXNtYQAEAAkAAAAGUGxhc21hAAQACQAAAAZQbGFzbWEABAAJAAAABlBsYXNtYQAEAAkAAAAG
    UGxhc21hAAQACQAAAAZQbGFzbWEAAAAQAAAAFwAAAAn/////AAAACf////8AAAAJ/////wAA
    AAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAAAAn/////
    AAAACf////8AAAAJ/////wAEAAkAAAAFSHVtYW4ABAAJAAAABUh1bWFuAAQACQAAAAVIdW1h
    bgAEAAkAAAAFSHVtYW4ABAAJAAAABUh1bWFuAAQACQAAAAVIdW1hbgAEAAkAAAAFSHVtYW4A
    BAAJAAAABUh1bWFuAAQACQAAAAVIdW1hbgAEAAkAAAAFSHVtYW4ABAAJAAAABUh1bWFuAAAA
    EAAAABcAAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8A
    AAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8ABAAJAAAA
    BE1BTEUABAAJAAAABE1BTEUABAAJAAAABE1BTEUABAAJAAAABE1BTEUABAAJAAAABE1BTEUA
    BAAJAAAABE1BTEUABAAJAAAABE1BTEUABAAJAAAABE1BTEUABAAJAAAABE1BTEUABAAJAAAA
    BE1BTEUABAAJAAAABE1BTEUAAAAQAAAAFwAAAAn/////AAAACf////8AAAAJ/////wAAAAn/
    ////AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAA
    Cf////8AAAAJ/////wAEAAkAAAAJQWNpY2xvdmlyAAQACQAAAAlBY2ljbG92aXIABAAJAAAA
    CUFjaWNsb3ZpcgAEAAkAAAAJQWNpY2xvdmlyAAQACQAAAAlBY2ljbG92aXIABAAJAAAACUFj
    aWNsb3ZpcgAEAAkAAAAJQWNpY2xvdmlyAAQACQAAAAlBY2ljbG92aXIABAAJAAAACUFjaWNs
    b3ZpcgAEAAkAAAAJQWNpY2xvdmlyAAQACQAAAAlBY2ljbG92aXIAAAAQAAAAFwAAAAn/////
    AAAACf////8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAAAAn/////AAAACf//
    //8AAAAJ/////wAAAAn/////AAAACf////8AAAAJ/////wAEAAkAAAACaXYABAAJAAAAAml2
    AAQACQAAAAJpdgAEAAkAAAACaXYABAAJAAAAAml2AAQACQAAAAJpdgAEAAkAAAACaXYABAAJ
    AAAAAml2AAQACQAAAAJpdgAEAAkAAAACaXYABAAJAAAAAml2AAAEAgAAAAEABAAJAAAACXJv
    dy5uYW1lcwAAAA0AAAACgAAAAP///+kAAAQCAAAAAQAEAAkAAAAFbmFtZXMAAAAQAAAAGAAE
    AAkAAAAMSW5kaXZpZHVhbElkAAQACQAAAAd4VmFsdWVzAAQACQAAAARuYW1lAAQACQAAAAd5
    VmFsdWVzAAQACQAAAAp4RGltZW5zaW9uAAQACQAAAAV4VW5pdAAEAAkAAAAKeURpbWVuc2lv
    bgAEAAkAAAAFeVVuaXQABAAJAAAACW1vbFdlaWdodAAEAAkAAAAIZGF0YVR5cGUABAAJAAAA
    DHlFcnJvclZhbHVlcwAEAAkAAAAFZ3JvdXAABAAJAAAACnlFcnJvclR5cGUABAAJAAAACnlF
    cnJvclVuaXQABAAJAAAABGxsb3EABAAJAAAABVNoZWV0AAQACQAAAAhTdHVkeSBJZAAEAAkA
    AAAKU3ViamVjdCBJZAAEAAkAAAAFT3JnYW4ABAAJAAAAC0NvbXBhcnRtZW50AAQACQAAAAdT
    cGVjaWVzAAQACQAAAAZHZW5kZXIABAAJAAAACE1vbGVjdWxlAAQACQAAAAVSb3V0ZQAABAIA
    AAABAAQACQAAAARzcGVjAAADEwAAAAMAAAITAAAAGAAAAxMAAAAAAAAEAgAAAAEABAAJAAAA
    BWNsYXNzAAAAEAAAAAIABAAJAAAAEGNvbGxlY3Rvcl9kb3VibGUABAAJAAAACWNvbGxlY3Rv
    cgAAAP4AAAMTAAAAAAAABAIAAAT/AAAAEAAAAAIABAAJAAAAEGNvbGxlY3Rvcl9kb3VibGUA
    BAAJAAAACWNvbGxlY3RvcgAAAP4AAAMTAAAAAAAABAIAAAT/AAAAEAAAAAIABAAJAAAAE2Nv
    bGxlY3Rvcl9jaGFyYWN0ZXIABAAJAAAACWNvbGxlY3RvcgAAAP4AAAMTAAAAAAAABAIAAAT/
    AAAAEAAAAAIABAAJAAAAEGNvbGxlY3Rvcl9kb3VibGUABAAJAAAACWNvbGxlY3RvcgAAAP4A
    AAMTAAAAAAAABAIAAAT/AAAAEAAAAAIABAAJAAAAE2NvbGxlY3Rvcl9jaGFyYWN0ZXIABAAJ
    AAAACWNvbGxlY3RvcgAAAP4AAAMTAAAAAAAABAIAAAT/AAAAEAAAAAIABAAJAAAAE2NvbGxl
    Y3Rvcl9jaGFyYWN0ZXIABAAJAAAACWNvbGxlY3RvcgAAAP4AAAMTAAAAAAAABAIAAAT/AAAA
    EAAAAAIABAAJAAAAE2NvbGxlY3Rvcl9jaGFyYWN0ZXIABAAJAAAACWNvbGxlY3RvcgAAAP4A
    AAMTAAAAAAAABAIAAAT/AAAAEAAAAAIABAAJAAAAE2NvbGxlY3Rvcl9jaGFyYWN0ZXIABAAJ
    AAAACWNvbGxlY3RvcgAAAP4AAAMTAAAAAAAABAIAAAT/AAAAEAAAAAIABAAJAAAAEGNvbGxl
    Y3Rvcl9kb3VibGUABAAJAAAACWNvbGxlY3RvcgAAAP4AAAMTAAAAAAAABAIAAAT/AAAAEAAA
    AAIABAAJAAAAE2NvbGxlY3Rvcl9jaGFyYWN0ZXIABAAJAAAACWNvbGxlY3RvcgAAAP4AAAMT
    AAAAAAAABAIAAAT/AAAAEAAAAAIABAAJAAAAEGNvbGxlY3Rvcl9kb3VibGUABAAJAAAACWNv
    bGxlY3RvcgAAAP4AAAMTAAAAAAAABAIAAAT/AAAAEAAAAAIABAAJAAAAE2NvbGxlY3Rvcl9j
    aGFyYWN0ZXIABAAJAAAACWNvbGxlY3RvcgAAAP4AAAMTAAAAAAAABAIAAAT/AAAAEAAAAAIA
    BAAJAAAAE2NvbGxlY3Rvcl9jaGFyYWN0ZXIABAAJAAAACWNvbGxlY3RvcgAAAP4AAAMTAAAA
    AAAABAIAAAT/AAAAEAAAAAIABAAJAAAAE2NvbGxlY3Rvcl9jaGFyYWN0ZXIABAAJAAAACWNv
    bGxlY3RvcgAAAP4AAAMTAAAAAAAABAIAAAT/AAAAEAAAAAIABAAJAAAAEWNvbGxlY3Rvcl9s
    b2dpY2FsAAQACQAAAAljb2xsZWN0b3IAAAD+AAADEwAAAAAAAAQCAAAE/wAAABAAAAACAAQA
    CQAAABNjb2xsZWN0b3JfY2hhcmFjdGVyAAQACQAAAAljb2xsZWN0b3IAAAD+AAADEwAAAAAA
    AAQCAAAE/wAAABAAAAACAAQACQAAABNjb2xsZWN0b3JfY2hhcmFjdGVyAAQACQAAAAljb2xs
    ZWN0b3IAAAD+AAADEwAAAAAAAAQCAAAE/wAAABAAAAACAAQACQAAABBjb2xsZWN0b3JfZG91
    YmxlAAQACQAAAAljb2xsZWN0b3IAAAD+AAADEwAAAAAAAAQCAAAE/wAAABAAAAACAAQACQAA
    ABNjb2xsZWN0b3JfY2hhcmFjdGVyAAQACQAAAAljb2xsZWN0b3IAAAD+AAADEwAAAAAAAAQC
    AAAE/wAAABAAAAACAAQACQAAABNjb2xsZWN0b3JfY2hhcmFjdGVyAAQACQAAAAljb2xsZWN0
    b3IAAAD+AAADEwAAAAAAAAQCAAAE/wAAABAAAAACAAQACQAAABNjb2xsZWN0b3JfY2hhcmFj
    dGVyAAQACQAAAAljb2xsZWN0b3IAAAD+AAADEwAAAAAAAAQCAAAE/wAAABAAAAACAAQACQAA
    ABNjb2xsZWN0b3JfY2hhcmFjdGVyAAQACQAAAAljb2xsZWN0b3IAAAD+AAADEwAAAAAAAAQC
    AAAE/wAAABAAAAACAAQACQAAABNjb2xsZWN0b3JfY2hhcmFjdGVyAAQACQAAAAljb2xsZWN0
    b3IAAAD+AAADEwAAAAAAAAQCAAAE/wAAABAAAAACAAQACQAAABNjb2xsZWN0b3JfY2hhcmFj
    dGVyAAQACQAAAAljb2xsZWN0b3IAAAD+AAAEAgAAAv8AAAAQAAAAGAAEAAkAAAAMSW5kaXZp
    ZHVhbElkAAQACQAAAAd4VmFsdWVzAAQACQAAAARuYW1lAAQACQAAAAd5VmFsdWVzAAQACQAA
    AAp4RGltZW5zaW9uAAQACQAAAAV4VW5pdAAEAAkAAAAKeURpbWVuc2lvbgAEAAkAAAAFeVVu
    aXQABAAJAAAACW1vbFdlaWdodAAEAAkAAAAIZGF0YVR5cGUABAAJAAAADHlFcnJvclZhbHVl
    cwAEAAkAAAAFZ3JvdXAABAAJAAAACnlFcnJvclR5cGUABAAJAAAACnlFcnJvclVuaXQABAAJ
    AAAABGxsb3EABAAJAAAABVNoZWV0AAQACQAAAAhTdHVkeSBJZAAEAAkAAAAKU3ViamVjdCBJ
    ZAAEAAkAAAAFT3JnYW4ABAAJAAAAC0NvbXBhcnRtZW50AAQACQAAAAdTcGVjaWVzAAQACQAA
    AAZHZW5kZXIABAAJAAAACE1vbGVjdWxlAAQACQAAAAVSb3V0ZQAAAP4AAAMTAAAAAAAABAIA
    AAT/AAAAEAAAAAIABAAJAAAAD2NvbGxlY3Rvcl9ndWVzcwAEAAkAAAAJY29sbGVjdG9yAAAA
    /gAAABAAAAABAAQACQAAAAEsAAAEAgAAAv8AAAAQAAAAAwAEAAkAAAAEY29scwAEAAkAAAAH
    ZGVmYXVsdAAEAAkAAAAFZGVsaW0AAAQCAAAE/wAAABAAAAABAAQACQAAAAhjb2xfc3BlYwAA
    AP4AAAQCAAAAAQAEAAkAAAAIcHJvYmxlbXMAAAAWAAAA/gAAAP4AAAQCAAAE/wAAABAAAAAE
    AAQACQAAAAtzcGVjX3RibF9kZgAEAAkAAAAGdGJsX2RmAAQACQAAAAN0YmwABAAJAAAACmRh
    dGEuZnJhbWUAAAD+

# `calculateCostMetrics` returns correct cost metric values for default parameters

    WAoAAAACAAQDAgACAwAAAAMTAAAABAAAAA4AAAABQIUrH0zc2DoAAAAOAAAAAUB1zNr+qwJT
    AAADEwAAAAcAAAAOAAAAAT/wAAAAAAAAAAAADQAAAAEAAAALAAAADgAAAAEAAAAAAAAAAAAA
    AA4AAAABQIUrH0zc2DoAAAAOAAAAAUCFKx9M3Ng6AAAADgAAAAFAhSsfTNzYOgAAAA4AAAAB
    QIUrH0zc2DoAAAQCAAAAAQAEAAkAAAAFbmFtZXMAAAAQAAAABwAEAAkAAAALc2NhbGVGYWN0
    b3IABAAJAAAADW5PYnNlcnZhdGlvbnMABAAJAAAADk0zQ29udHJpYnV0aW9uAAQACQAAAANT
    U1IABAAJAAAAC3dlaWdodGVkU1NSAAQACQAAAA1ub3JtYWxpemVkU1NSAAQACQAAAAlyb2J1
    c3RTU1IAAAQCAAAAAQAEAAkAAAAFY2xhc3MAAAAQAAAAAQAEAAkAAAAKZGF0YS5mcmFtZQAA
    BAIAAAABAAQACQAAAAlyb3cubmFtZXMAAAANAAAAAoAAAAD/////AAAA/gAAAxMAAAAIAAAA
    DgAAAAtAMF0XX+38R0A9F0XgCRSnQEa6Lp/7iHdAUUXRQAPRG0BYF0ZAAEspQF2Lov/fSNBA
    ZoAAAAAAAEBy3RdACC3MQHpAAAAAAABAhJF0v/stMkCR10Wf9P1tAAAADgAAAAtAJQy3q31x
    xkAqKnCyxY+XQCtUWMpxfcdAK1RYynF9x0AjS5SdDiDeQCEuap+oc8lAFzpPrSKeokAKAXhB
    fJl7P/7a6vkY7EY/51nsQudj2j++CjuHA4OyAAAADgAAAAtAQeIXIAIo+kAy0OMAFPi2QCun
    fQAF3z1AJ8ZCQBZwhUAk5Yi/15veQCLnUH/9xMVAHIs0P/iTekAQg7I/+w8iQAOMiQAIJ1s/
    6667wD2l2z+9D/zAYw+iAAAADgAAAAs/8AAAAAAAAD/wAAAAAAAAP/AAAAAAAAA/8AAAAAAA
    AD/wAAAAAAAAP/AAAAAAAAA/8AAAAAAAAD/wAAAAAAAAP/AAAAAAAAA/8AAAAAAAAD/wAAAA
    AAAAAAAADgAAAAtAOT3SakWZEUAW7qqayMOqP8TJDWUYXYC//HC0UthqED/pn0Isl7AAP+uO
    XgVVD8A/9UOSS1fTYD/sF7D55hMkP+B8Tg3uxOA/wVM99VkIBL9vR9jUDoIAAAAADgAAAAtA
    OT3SakWZEUAW7qqayMOqP8TJDWUYXYC//HC0UthqED/pn0Isl7AAP+uOXgVVD8A/9UOSS1fT
    YD/sF7D55hMkP+B8Tg3uxOA/wVM99VkIBL9vR9jUDoIAAAAADgAAAAtAOT3SakWZEUAW7qqa
    yMOqP8TJDWUYXYC//HC0UthqED/pn0Isl7AAP+uOXgVVD8A/9UOSS1fTYD/sF7D55hMkP+B8
    Tg3uxOA/wVM99VkIBL9vR9jUDoIAAAAADgAAAAtAOT3SakWZEUAW7qqayMOqP8TJDWUYXYC/
    /HC0UthqED/pn0Isl7AAP+uOXgVVD8A/9UOSS1fTYD/sF7D55hMkP+B8Tg3uxOA/wVM99VkI
    BL9vR9jUDoIAAAAEAgAAAf8AAAAQAAAACAAEAAkAAAABeAAEAAkAAAAJeU9ic2VydmVkAAQA
    CQAAAAp5U2ltdWxhdGVkAAQACQAAAAZ3ZWlnaHQABAAJAAAACXJlc2lkdWFscwAEAAkAAAAR
    d2VpZ2h0ZWRSZXNpZHVhbHMABAAJAAAAE25vcm1hbGl6ZWRSZXNpZHVhbHMABAAJAAAAF3Jv
    YnVzdFdlaWdodGVkUmVzaWR1YWxzAAAEAgAAAv8AAAAQAAAAAQAEAAkAAAAKZGF0YS5mcmFt
    ZQAABAIAAAP/AAAADQAAAAKAAAAA////9QAAAP4AAAQCAAAB/wAAABAAAAAEAAQACQAAAAlt
    b2RlbENvc3QABAAJAAAAEW1pbkxvZ1Byb2JhYmlsaXR5AAQACQAAAA1jb3N0VmFyaWFibGVz
    AAQACQAAAA9yZXNpZHVhbERldGFpbHMAAAQCAAAC/wAAABAAAAABAAQACQAAAAltb2RlbENv
    c3QAAAD+

# different residual weighting methods affect the residuals

    WAoAAAACAAQDAgACAwAAAAATAAAABAAAAxMAAAAEAAAADgAAAAFAhSsfTNzYOgAAAA4AAAAB
    QHXM2v6rAlMAAAMTAAAABwAAAA4AAAABP/AAAAAAAAAAAAANAAAAAQAAAAsAAAAOAAAAAQAA
    AAAAAAAAAAAADgAAAAFAhSsfTNzYOgAAAA4AAAABQIUrH0zc2DoAAAAOAAAAAUCFKx9M3Ng6
    AAAADgAAAAFAhSsfTNzYOgAABAIAAAABAAQACQAAAAVuYW1lcwAAABAAAAAHAAQACQAAAAtz
    Y2FsZUZhY3RvcgAEAAkAAAANbk9ic2VydmF0aW9ucwAEAAkAAAAOTTNDb250cmlidXRpb24A
    BAAJAAAAA1NTUgAEAAkAAAALd2VpZ2h0ZWRTU1IABAAJAAAADW5vcm1hbGl6ZWRTU1IABAAJ
    AAAACXJvYnVzdFNTUgAABAIAAAABAAQACQAAAAVjbGFzcwAAABAAAAABAAQACQAAAApkYXRh
    LmZyYW1lAAAEAgAAAAEABAAJAAAACXJvdy5uYW1lcwAAAA0AAAACgAAAAP////8AAAD+AAAD
    EwAAAAgAAAAOAAAAC0AwXRdf7fxHQD0XReAJFKdARroun/uId0BRRdFAA9EbQFgXRkAASylA
    XYui/99I0EBmgAAAAAAAQHLdF0AILcxAekAAAAAAAECEkXS/+y0yQJHXRZ/0/W0AAAAOAAAA
    C0AlDLerfXHGQCoqcLLFj5dAK1RYynF9x0ArVFjKcX3HQCNLlJ0OIN5AIS5qn6hzyUAXOk+t
    Ip6iQAoBeEF8mXs//trq+RjsRj/nWexC52PaP74KO4cDg7IAAAAOAAAAC0BB4hcgAij6QDLQ
    4wAU+LZAK6d9AAXfPUAnxkJAFnCFQCTliL/Xm95AIudQf/3ExUAcizQ/+JN6QBCDsj/7DyJA
    A4yJAAgnWz/rrrvAPaXbP70P/MBjD6IAAAAOAAAACz/wAAAAAAAAP/AAAAAAAAA/8AAAAAAA
    AD/wAAAAAAAAP/AAAAAAAAA/8AAAAAAAAD/wAAAAAAAAP/AAAAAAAAA/8AAAAAAAAD/wAAAA
    AAAAP/AAAAAAAAAAAAAOAAAAC0A5PdJqRZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS2GoQP+mf
    QiyXsAA/645eBVUPwD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAA
    AAAOAAAAC0A5PdJqRZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS2GoQP+mfQiyXsAA/645eBVUP
    wD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAAAAAOAAAAC0A5PdJq
    RZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS2GoQP+mfQiyXsAA/645eBVUPwD/1Q5JLV9NgP+wX
    sPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAAAAAOAAAAC0A5PdJqRZkRQBbuqprIw6o/
    xMkNZRhdgL/8cLRS2GoQP+mfQiyXsAA/645eBVUPwD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E
    4D/BUz31WQgEv29H2NQOggAAAAQCAAAB/wAAABAAAAAIAAQACQAAAAF4AAQACQAAAAl5T2Jz
    ZXJ2ZWQABAAJAAAACnlTaW11bGF0ZWQABAAJAAAABndlaWdodAAEAAkAAAAJcmVzaWR1YWxz
    AAQACQAAABF3ZWlnaHRlZFJlc2lkdWFscwAEAAkAAAATbm9ybWFsaXplZFJlc2lkdWFscwAE
    AAkAAAAXcm9idXN0V2VpZ2h0ZWRSZXNpZHVhbHMAAAQCAAAC/wAAABAAAAABAAQACQAAAApk
    YXRhLmZyYW1lAAAEAgAAA/8AAAANAAAAAoAAAAD////1AAAA/gAABAIAAAH/AAAAEAAAAAQA
    BAAJAAAACW1vZGVsQ29zdAAEAAkAAAARbWluTG9nUHJvYmFiaWxpdHkABAAJAAAADWNvc3RW
    YXJpYWJsZXMABAAJAAAAD3Jlc2lkdWFsRGV0YWlscwAABAIAAAL/AAAAEAAAAAEABAAJAAAA
    CW1vZGVsQ29zdAAAAP4AAAMTAAAABAAAAA4AAAABQDizkMtfpCEAAAAOAAAAAUBEVfkU9sHC
    AAADEwAAAAcAAAAOAAAAAT/wAAAAAAAAAAAADQAAAAEAAAALAAAADgAAAAEAAAAAAAAAAAAA
    AA4AAAABQIUrH0zc2DoAAAAOAAAAAUA4s5DLX6QhAAAADgAAAAFAOLOQy1+kIQAAAA4AAAAB
    QDizkMtfpCEAAAQCAAAB/wAAABAAAAAHAAQACQAAAAtzY2FsZUZhY3RvcgAEAAkAAAANbk9i
    c2VydmF0aW9ucwAEAAkAAAAOTTNDb250cmlidXRpb24ABAAJAAAAA1NTUgAEAAkAAAALd2Vp
    Z2h0ZWRTU1IABAAJAAAADW5vcm1hbGl6ZWRTU1IABAAJAAAACXJvYnVzdFNTUgAABAIAAAL/
    AAAAEAAAAAEABAAJAAAACmRhdGEuZnJhbWUAAAQCAAAD/wAAAA0AAAACgAAAAP////8AAAD+
    AAADEwAAAAgAAAAOAAAAC0AwXRdf7fxHQD0XReAJFKdARroun/uId0BRRdFAA9EbQFgXRkAA
    SylAXYui/99I0EBmgAAAAAAAQHLdF0AILcxAekAAAAAAAECEkXS/+y0yQJHXRZ/0/W0AAAAO
    AAAAC0AlDLerfXHGQCoqcLLFj5dAK1RYynF9x0ArVFjKcX3HQCNLlJ0OIN5AIS5qn6hzyUAX
    Ok+tIp6iQAoBeEF8mXs//trq+RjsRj/nWexC52PaP74KO4cDg7IAAAAOAAAAC0BB4hcgAij6
    QDLQ4wAU+LZAK6d9AAXfPUAnxkJAFnCFQCTliL/Xm95AIudQf/3ExUAcizQ/+JN6QBCDsj/7
    DyJAA4yJAAgnWz/rrrvAPaXbP70P/MBjD6IAAAAOAAAACz/IcV1tWmVBP8hxXW1aZUE/yHFd
    bVplQT/IcV1tWmVBP8hxXW1aZUE/yHFdbVplQT/IcV1tWmVBP8hxXW1aZUE/yHFdbVplQT/I
    cV1tWmVBP8hxXW1aZUEAAAAOAAAAC0A5PdJqRZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS2GoQ
    P+mfQiyXsAA/645eBVUPwD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQO
    ggAAAAAOAAAAC0ATR8nQqN/jP/GEPbKGtnk/n8DZZOt9p7/VuUhDoym6P8OSNtGGZ6M/xQxl
    s+6nij/QPgJqJ1NGP8V1Smk5Z2w/uS9DP1o15D+ad53/22rcv0fks6hq9KMAAAAOAAAAC0AT
    R8nQqN/jP/GEPbKGtnk/n8DZZOt9p7/VuUhDoym6P8OSNtGGZ6M/xQxls+6nij/QPgJqJ1NG
    P8V1Smk5Z2w/uS9DP1o15D+ad53/22rcv0fks6hq9KMAAAAOAAAAC0ATR8nQqN/jP/GEPbKG
    tnk/n8DZZOt9p7/VuUhDoym6P8OSNtGGZ6M/xQxls+6nij/QPgJqJ1NGP8V1Smk5Z2w/uS9D
    P1o15D+ad53/22rcv0fks6hq9KMAAAQCAAAB/wAAABAAAAAIAAQACQAAAAF4AAQACQAAAAl5
    T2JzZXJ2ZWQABAAJAAAACnlTaW11bGF0ZWQABAAJAAAABndlaWdodAAEAAkAAAAJcmVzaWR1
    YWxzAAQACQAAABF3ZWlnaHRlZFJlc2lkdWFscwAEAAkAAAATbm9ybWFsaXplZFJlc2lkdWFs
    cwAEAAkAAAAXcm9idXN0V2VpZ2h0ZWRSZXNpZHVhbHMAAAQCAAAC/wAAABAAAAABAAQACQAA
    AApkYXRhLmZyYW1lAAAEAgAAA/8AAAANAAAAAoAAAAD////1AAAA/gAABAIAAAH/AAAAEAAA
    AAQABAAJAAAACW1vZGVsQ29zdAAEAAkAAAARbWluTG9nUHJvYmFiaWxpdHkABAAJAAAADWNv
    c3RWYXJpYWJsZXMABAAJAAAAD3Jlc2lkdWFsRGV0YWlscwAABAIAAAL/AAAAEAAAAAEABAAJ
    AAAACW1vZGVsQ29zdAAAAP4AAAMTAAAABAAAAA4AAAABQCj63VCdsrYAAAAOAAAAAUBDKIWh
    OTBOAAADEwAAAAcAAAAOAAAAAT/wAAAAAAAAAAAADQAAAAEAAAALAAAADgAAAAEAAAAAAAAA
    AAAAAA4AAAABQIUrH0zc2DoAAAAOAAAAAUAo+t1QnbK2AAAADgAAAAFAKPrdUJ2ytgAAAA4A
    AAABQCj63VCdsrYAAAQCAAAB/wAAABAAAAAHAAQACQAAAAtzY2FsZUZhY3RvcgAEAAkAAAAN
    bk9ic2VydmF0aW9ucwAEAAkAAAAOTTNDb250cmlidXRpb24ABAAJAAAAA1NTUgAEAAkAAAAL
    d2VpZ2h0ZWRTU1IABAAJAAAADW5vcm1hbGl6ZWRTU1IABAAJAAAACXJvYnVzdFNTUgAABAIA
    AAL/AAAAEAAAAAEABAAJAAAACmRhdGEuZnJhbWUAAAQCAAAD/wAAAA0AAAACgAAAAP////8A
    AAD+AAADEwAAAAgAAAAOAAAAC0AwXRdf7fxHQD0XReAJFKdARroun/uId0BRRdFAA9EbQFgX
    RkAASylAXYui/99I0EBmgAAAAAAAQHLdF0AILcxAekAAAAAAAECEkXS/+y0yQJHXRZ/0/W0A
    AAAOAAAAC0AlDLerfXHGQCoqcLLFj5dAK1RYynF9x0ArVFjKcX3HQCNLlJ0OIN5AIS5qn6hz
    yUAXOk+tIp6iQAoBeEF8mXs//trq+RjsRj/nWexC52PaP74KO4cDg7IAAAAOAAAAC0BB4hcg
    Aij6QDLQ4wAU+LZAK6d9AAXfPUAnxkJAFnCFQCTliL/Xm95AIudQf/3ExUAcizQ/+JN6QBCD
    sj/7DyJAA4yJAAgnWz/rrrvAPaXbP70P/MBjD6IAAAAOAAAACz/BYX/U21i1P8Fhf9TbWLU/
    wWF/1NtYtT/BYX/U21i1P8Fhf9TbWLU/wWF/1NtYtT/BYX/U21i1P8Fhf9TbWLU/wWF/1NtY
    tT/BYX/U21i1P8Fhf9TbWLUAAAAOAAAAC0A5PdJqRZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS
    2GoQP+mfQiyXsAA/645eBVUPwD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H
    2NQOggAAAAAOAAAAC0ALa4AHEZI8P+jpU2Y+lAc/lpRHLP3+IL/O5Q4WLrvlP7vVWIWKM0Y/
    ve8vKE0h8z/HGV76MiA+P76EXBw3P7I/seiIPf7CWj+S0gTnhkgYv0D9ee8hcYsAAAAOAAAA
    C0ALa4AHEZI8P+jpU2Y+lAc/lpRHLP3+IL/O5Q4WLrvlP7vVWIWKM0Y/ve8vKE0h8z/HGV76
    MiA+P76EXBw3P7I/seiIPf7CWj+S0gTnhkgYv0D9ee8hcYsAAAAOAAAAC0ALa4AHEZI8P+jp
    U2Y+lAc/lpRHLP3+IL/O5Q4WLrvlP7vVWIWKM0Y/ve8vKE0h8z/HGV76MiA+P76EXBw3P7I/
    seiIPf7CWj+S0gTnhkgYv0D9ee8hcYsAAAQCAAAB/wAAABAAAAAIAAQACQAAAAF4AAQACQAA
    AAl5T2JzZXJ2ZWQABAAJAAAACnlTaW11bGF0ZWQABAAJAAAABndlaWdodAAEAAkAAAAJcmVz
    aWR1YWxzAAQACQAAABF3ZWlnaHRlZFJlc2lkdWFscwAEAAkAAAATbm9ybWFsaXplZFJlc2lk
    dWFscwAEAAkAAAAXcm9idXN0V2VpZ2h0ZWRSZXNpZHVhbHMAAAQCAAAC/wAAABAAAAABAAQA
    CQAAAApkYXRhLmZyYW1lAAAEAgAAA/8AAAANAAAAAoAAAAD////1AAAA/gAABAIAAAH/AAAA
    EAAAAAQABAAJAAAACW1vZGVsQ29zdAAEAAkAAAARbWluTG9nUHJvYmFiaWxpdHkABAAJAAAA
    DWNvc3RWYXJpYWJsZXMABAAJAAAAD3Jlc2lkdWFsRGV0YWlscwAABAIAAAL/AAAAEAAAAAEA
    BAAJAAAACW1vZGVsQ29zdAAAAP4AAAMTAAAABAAAAA4AAAABQIQT8fIQM/YAAAAOAAAAAUB1
    Dgg7g3/+AAADEwAAAAcAAAAOAAAAAT/wAAAAAAAAAAAADQAAAAEAAAALAAAADgAAAAEAAAAA
    AAAAAAAAAA4AAAABQIUrH0zc2DoAAAAOAAAAAUCEE/HyEDP2AAAADgAAAAFAhBPx8hAz9gAA
    AA4AAAABQIQT8fIQM/YAAAQCAAAB/wAAABAAAAAHAAQACQAAAAtzY2FsZUZhY3RvcgAEAAkA
    AAANbk9ic2VydmF0aW9ucwAEAAkAAAAOTTNDb250cmlidXRpb24ABAAJAAAAA1NTUgAEAAkA
    AAALd2VpZ2h0ZWRTU1IABAAJAAAADW5vcm1hbGl6ZWRTU1IABAAJAAAACXJvYnVzdFNTUgAA
    BAIAAAL/AAAAEAAAAAEABAAJAAAACmRhdGEuZnJhbWUAAAQCAAAD/wAAAA0AAAACgAAAAP//
    //8AAAD+AAADEwAAAAgAAAAOAAAAC0AwXRdf7fxHQD0XReAJFKdARroun/uId0BRRdFAA9Eb
    QFgXRkAASylAXYui/99I0EBmgAAAAAAAQHLdF0AILcxAekAAAAAAAECEkXS/+y0yQJHXRZ/0
    /W0AAAAOAAAAC0AlDLerfXHGQCoqcLLFj5dAK1RYynF9x0ArVFjKcX3HQCNLlJ0OIN5AIS5q
    n6hzyUAXOk+tIp6iQAoBeEF8mXs//trq+RjsRj/nWexC52PaP74KO4cDg7IAAAAOAAAAC0BB
    4hcgAij6QDLQ4wAU+LZAK6d9AAXfPUAnxkJAFnCFQCTliL/Xm95AIudQf/3ExUAcizQ/+JN6
    QBCDsj/7DyJAA4yJAAgnWz/rrrvAPaXbP70P/MBjD6IAAAAOAAAACz/wAAAAAAAAP8d/Op4w
    sns/xn8ZatBD4j+/t9mdKN/JP/AAAAAAAAA/8AAAAAAAAD/wAAAAAAAAP/AAAAAAAAA/8AAA
    AAAAAD/wAAAAAAAAP/AAAAAAAAAAAAAOAAAAC0A5PdJqRZkRQBbuqprIw6o/xMkNZRhdgL/8
    cLRS2GoQP+mfQiyXsAA/645eBVUPwD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgE
    v29H2NQOggAAAAAOAAAAC0A5PdJqRZkRP/DWt9ZJIjo/nTmPSjtmh7/MMJSfb4amP+mfQiyX
    sAA/645eBVUPwD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAAAAAO
    AAAAC0A5PdJqRZkRP/DWt9ZJIjo/nTmPSjtmh7/MMJSfb4amP+mfQiyXsAA/645eBVUPwD/1
    Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAAAAAOAAAAC0A5PdJqRZkR
    P/DWt9ZJIjo/nTmPSjtmh7/MMJSfb4amP+mfQiyXsAA/645eBVUPwD/1Q5JLV9NgP+wXsPnm
    EyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAAAAQCAAAB/wAAABAAAAAIAAQACQAAAAF4AAQA
    CQAAAAl5T2JzZXJ2ZWQABAAJAAAACnlTaW11bGF0ZWQABAAJAAAABndlaWdodAAEAAkAAAAJ
    cmVzaWR1YWxzAAQACQAAABF3ZWlnaHRlZFJlc2lkdWFscwAEAAkAAAATbm9ybWFsaXplZFJl
    c2lkdWFscwAEAAkAAAAXcm9idXN0V2VpZ2h0ZWRSZXNpZHVhbHMAAAQCAAAC/wAAABAAAAAB
    AAQACQAAAApkYXRhLmZyYW1lAAAEAgAAA/8AAAANAAAAAoAAAAD////1AAAA/gAABAIAAAH/
    AAAAEAAAAAQABAAJAAAACW1vZGVsQ29zdAAEAAkAAAARbWluTG9nUHJvYmFiaWxpdHkABAAJ
    AAAADWNvc3RWYXJpYWJsZXMABAAJAAAAD3Jlc2lkdWFsRGV0YWlscwAABAIAAAL/AAAAEAAA
    AAEABAAJAAAACW1vZGVsQ29zdAAAAP4=

# robust methods (huber, bisquare) modify the residuals appropriately

    WAoAAAACAAQDAgACAwAAAAATAAAAAgAAAxMAAAAEAAAADgAAAAFAIeNO34BcXwAAAA4AAAAB
    QDNzHgM8EEgAAAMTAAAABwAAAA4AAAABP/AAAAAAAAAAAAANAAAAAQAAAAsAAAAOAAAAAQAA
    AAAAAAAAAAAADgAAAAFAhSsfTNzYOgAAAA4AAAABQIUrH0zc2DoAAAAOAAAAAUCFKx9M3Ng6
    AAAADgAAAAFAIeNO34BcXwAABAIAAAABAAQACQAAAAVuYW1lcwAAABAAAAAHAAQACQAAAAtz
    Y2FsZUZhY3RvcgAEAAkAAAANbk9ic2VydmF0aW9ucwAEAAkAAAAOTTNDb250cmlidXRpb24A
    BAAJAAAAA1NTUgAEAAkAAAALd2VpZ2h0ZWRTU1IABAAJAAAADW5vcm1hbGl6ZWRTU1IABAAJ
    AAAACXJvYnVzdFNTUgAABAIAAAABAAQACQAAAAVjbGFzcwAAABAAAAABAAQACQAAAApkYXRh
    LmZyYW1lAAAEAgAAAAEABAAJAAAACXJvdy5uYW1lcwAAAA0AAAACgAAAAP////8AAAD+AAAD
    EwAAAAgAAAAOAAAAC0AwXRdf7fxHQD0XReAJFKdARroun/uId0BRRdFAA9EbQFgXRkAASylA
    XYui/99I0EBmgAAAAAAAQHLdF0AILcxAekAAAAAAAECEkXS/+y0yQJHXRZ/0/W0AAAAOAAAA
    C0AlDLerfXHGQCoqcLLFj5dAK1RYynF9x0ArVFjKcX3HQCNLlJ0OIN5AIS5qn6hzyUAXOk+t
    Ip6iQAoBeEF8mXs//trq+RjsRj/nWexC52PaP74KO4cDg7IAAAAOAAAAC0BB4hcgAij6QDLQ
    4wAU+LZAK6d9AAXfPUAnxkJAFnCFQCTliL/Xm95AIudQf/3ExUAcizQ/+JN6QBCDsj/7DyJA
    A4yJAAgnWz/rrrvAPaXbP70P/MBjD6IAAAAOAAAACz+p0YN9cO+KP8xrEYbbSHc/8AAAAAAA
    AD/m6h87J8TQP/AAAAAAAAA/8AAAAAAAAD/updwv35kLP/AAAAAAAAA/8AAAAAAAAD/wAAAA
    AAAAP/AAAAAAAAAAAAAOAAAAC0A5PdJqRZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS2GoQP+mf
    QiyXsAA/645eBVUPwD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAA
    AAAOAAAAC0A5PdJqRZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS2GoQP+mfQiyXsAA/645eBVUP
    wD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAAAAAOAAAAC0A5PdJq
    RZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS2GoQP+mfQiyXsAA/645eBVUPwD/1Q5JLV9NgP+wX
    sPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAAAAAOAAAACz/0XY/hVGx0P/Rdj+FUbHQ/
    xMkNZRhdgL/0XY/hVGx0P+mfQiyXsAA/645eBVUPwD/0XY/hVGx0P+wXsPnmEyQ/4HxODe7E
    4D/BUz31WQgEv29H2NQOggAAAAQCAAAB/wAAABAAAAAIAAQACQAAAAF4AAQACQAAAAl5T2Jz
    ZXJ2ZWQABAAJAAAACnlTaW11bGF0ZWQABAAJAAAABndlaWdodAAEAAkAAAAJcmVzaWR1YWxz
    AAQACQAAABF3ZWlnaHRlZFJlc2lkdWFscwAEAAkAAAATbm9ybWFsaXplZFJlc2lkdWFscwAE
    AAkAAAAXcm9idXN0V2VpZ2h0ZWRSZXNpZHVhbHMAAAQCAAAC/wAAABAAAAABAAQACQAAAApk
    YXRhLmZyYW1lAAAEAgAAA/8AAAANAAAAAoAAAAD////1AAAA/gAABAIAAAH/AAAAEAAAAAQA
    BAAJAAAACW1vZGVsQ29zdAAEAAkAAAARbWluTG9nUHJvYmFiaWxpdHkABAAJAAAADWNvc3RW
    YXJpYWJsZXMABAAJAAAAD3Jlc2lkdWFsRGV0YWlscwAABAIAAAL/AAAAEAAAAAEABAAJAAAA
    CW1vZGVsQ29zdAAAAP4AAAMTAAAABAAAAA4AAAABQBO3xX4kxpYAAAAOAAAAAX/wAAAAAAAA
    AAADEwAAAAcAAAAOAAAAAT/wAAAAAAAAAAAADQAAAAEAAAALAAAADgAAAAEAAAAAAAAAAAAA
    AA4AAAABQIUrH0zc2DoAAAAOAAAAAUCFKx9M3Ng6AAAADgAAAAFAhSsfTNzYOgAAAA4AAAAB
    QBO3xX4kxpYAAAQCAAAB/wAAABAAAAAHAAQACQAAAAtzY2FsZUZhY3RvcgAEAAkAAAANbk9i
    c2VydmF0aW9ucwAEAAkAAAAOTTNDb250cmlidXRpb24ABAAJAAAAA1NTUgAEAAkAAAALd2Vp
    Z2h0ZWRTU1IABAAJAAAADW5vcm1hbGl6ZWRTU1IABAAJAAAACXJvYnVzdFNTUgAABAIAAAL/
    AAAAEAAAAAEABAAJAAAACmRhdGEuZnJhbWUAAAQCAAAD/wAAAA0AAAACgAAAAP////8AAAD+
    AAADEwAAAAgAAAAOAAAAC0AwXRdf7fxHQD0XReAJFKdARroun/uId0BRRdFAA9EbQFgXRkAA
    SylAXYui/99I0EBmgAAAAAAAQHLdF0AILcxAekAAAAAAAECEkXS/+y0yQJHXRZ/0/W0AAAAO
    AAAAC0AlDLerfXHGQCoqcLLFj5dAK1RYynF9x0ArVFjKcX3HQCNLlJ0OIN5AIS5qn6hzyUAX
    Ok+tIp6iQAoBeEF8mXs//trq+RjsRj/nWexC52PaP74KO4cDg7IAAAAOAAAAC0BB4hcgAij6
    QDLQ4wAU+LZAK6d9AAXfPUAnxkJAFnCFQCTliL/Xm95AIudQf/3ExUAcizQ/+JN6QBCDsj/7
    DyJAA4yJAAgnWz/rrrvAPaXbP70P/MBjD6IAAAAOAAAACwAAAAAAAAAAAAAAAAAAAAA/7+oJ
    XXJivT/mijOfgnAmP+3yXSDoPv8/7aGYZM8Y0z/qggNaGbMCP+2KO4xSn2c/7yRIpZy3PD/v
    8LzGW3YxP+///OOR6/8AAAAOAAAAC0A5PdJqRZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS2GoQ
    P+mfQiyXsAA/645eBVUPwD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQO
    ggAAAAAOAAAAC0A5PdJqRZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS2GoQP+mfQiyXsAA/645e
    BVUPwD/1Q5JLV9NgP+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAAAAAOAAAAC0A5
    PdJqRZkRQBbuqprIw6o/xMkNZRhdgL/8cLRS2GoQP+mfQiyXsAA/645eBVUPwD/1Q5JLV9Ng
    P+wXsPnmEyQ/4HxODe7E4D/BUz31WQgEv29H2NQOggAAAAAOAAAACwAAAAAAAAAAAAAAAAAA
    AAA/xLrJQSE8f7/0CE/j/V/CP+f6Yu4MO3k/6YQsuO2/sz/xnUuZvkbKP+nu0tSMrTY/4Asc
    4lBJij/BSvqKzvD/v29H1cmHtd4AAAQCAAAB/wAAABAAAAAIAAQACQAAAAF4AAQACQAAAAl5
    T2JzZXJ2ZWQABAAJAAAACnlTaW11bGF0ZWQABAAJAAAABndlaWdodAAEAAkAAAAJcmVzaWR1
    YWxzAAQACQAAABF3ZWlnaHRlZFJlc2lkdWFscwAEAAkAAAATbm9ybWFsaXplZFJlc2lkdWFs
    cwAEAAkAAAAXcm9idXN0V2VpZ2h0ZWRSZXNpZHVhbHMAAAQCAAAC/wAAABAAAAABAAQACQAA
    AApkYXRhLmZyYW1lAAAEAgAAA/8AAAANAAAAAoAAAAD////1AAAA/gAABAIAAAH/AAAAEAAA
    AAQABAAJAAAACW1vZGVsQ29zdAAEAAkAAAARbWluTG9nUHJvYmFiaWxpdHkABAAJAAAADWNv
    c3RWYXJpYWJsZXMABAAJAAAAD3Jlc2lkdWFsRGV0YWlscwAABAIAAAL/AAAAEAAAAAEABAAJ
    AAAACW1vZGVsQ29zdAAAAP4=
