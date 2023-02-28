# TGC_API
 Telegram Client for Delphi

Hello. The package is still being worked on. Authorization and full-fledged work with the client is done. You can already use the library, but I will implement more convenient methods and describe the Telegram classes.

A package is a non-visual component that you need to configure for yourself in the designer.
To work with a client, you need to have `ApiId` and `ApiHash` - the data of the application you registered on the Telegram website. `https://my.telegram.org/apps`
Next, you can customize the Parameters section to your liking.

The client requires the assembly of the `tdjson.dll` library version `1.8.11`, as well as the library: `libcrypto-1_1.dll`, `libssl-1_1.dll`, `zlib1.dll`.
Libraries are in the Client folder in the examples
