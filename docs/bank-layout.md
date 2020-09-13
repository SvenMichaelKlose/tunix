# Bank layout

## ROM

```
0: Boot loader + kernal
1-7: at user's disposal, options in boot loader
8-8191: file system
```

## RAM

```
0: desktop RAM1,2,3/IO
1: desktop BLK1
2: desktop BLK2
3: desktop BLK3
4: desktop BLK5
5: desktop UltiFS code
6: desktop 4x8 charset
7: desktop file window code
8-120: free
120-126: saved state
127: Ingle ROM function BSS.
```
