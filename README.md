# Backup Machine #
*The best backup machine in this World!* 

This is a powerful and wery smalland simple backup system, but it for power users only.

A backup system that has some amazing features that no one else has:

1. Saving all versions of files, i.e. if you work with a file for 2 hours in a row, all saved versions of this file (with adjustable frequency) will get into the backup; 
2. Using RAR for archiving, RAR 5 is the most powerful and functional of archivers, for example, you can configure backup with redundancy to be able to restore backups in case of disk damage;
3. Can by set users list for backup specific users file changes if work with shared folder;
4. A simple and small open source client (an EXE file can be opened in WinRAR as an archive with source codes).

How does the system work:
- After start program work silently in your tray and log all file changes at source directory, after every 5 minutes (delay period can be set in config) program start RAR in hide mode with special parameters line for archive only changed files (and with exclode mask) and with save previosly version of files in archive;
- In your back-up folder, files accumulate with changes on individual days.

**Релиз**
http://wyfinger.github.io/BackupMachine/

**Disclaimer**

It was previously written below:
> At this moment the program is in beta status, use at your own risk and see source..

But now, 2019/08/06 this system work 24/7 in our office since 2015/03/30, more than 4 years. There were no failures or denial of service. Today I can conclude that the program is ready for use!