round 22 years ago, I was writing music on the Commodore 64 home computer as an active participant in the C64 'Demoscene'. Most of my work from this era has been lost except for three cruddy pieces of music which can be found on the 'High-Voltage SID Collection' under the pseudo-name of 'Gazza'.

I shouldn't mention that because the tunes that have been recovered from various demos and people are the ones that I am least proud of. Still, it's better than having nothing preserved from that era, that's for sure!

Anyhow, I recently saw an attempt of disassembling the music driver of one of the C64 legends, Martin Galway and it inspired me to fill my holiday time by writing a music driver that allows for ultimate control and flexibility by removing the limitations of the way sounds (instruments) are defined.

People in the know will know that I'm not talking about something that hasn't been done before. Rather, my approach was to make every single register of the SID chip (even the ADSR envelope) subject to macro-driven modulation (often loosely called wave-) tables.

So, this is what I hoped to achieve with this driver:

* zSPL - "zBlex SID Programming Language". Extending on the usual C64 music driver commands (such as Instrument change, tempo change & portamento), I've added the ability to do For/Next style loops and jumps to subsequences (Macros or sub-routines).

Each voice has its own stack to track these execution jumps to a depth of eight. Obviously this depth can be changed at the expense of memory.

* zIPL - "zBlex Instrument Programming Language". Each property of a voice (Attack/Decay, Sustain/Release, Control, Frequency & Pulse Width) uses its own macro table each with its own capability for Do/While loops.

This means that you can change arpeggios mid-note or the speed and direction of a pulse/frequency sweep. You can even flutter the envelope for echo and volume effects. It is so flexible that you could theoretically write the entire tune in the instruments macros but you would want to be mad to actually attempt this.

* Six Voices - Yup, it can handle those crazy arrangements where you have two real SID chips or an emulator that lets you emulate two SIDs. One of the standard tricks in C64 music is to do a rapid arpeggio to simulate a chord. 

While this is a technique that uniquely identifies C64 music so it would be a shame to lose it, but with six sound channels, real chords can be created without sacrificing the rest of the musics elements. It also means that you now have TWO FILTERS - one per SID.

Not only does my music driver macro-table modulate EVERY SID register but it also does it for up to two SID chips! Yep! You can have a total of SIX sound channels with two fully functional sets of Filters.

Since most people experience the C64 nowadays via emulators (such as VICE or the SID emulatore jSIDPlay2), this is something that anyone can have running. It would be great to hear a six channel tune through two physical SIDs though I doubt many people have access to "the real deal".

Time is starting to be at a premium so I fear I wont get the opportunity to actually write a piece of music with this routine so I've decided to public the code for all to see. So, without further ado, here's what you need to know (coming soon, I promise): 

Documentation - Build environment and descriptions of the zSPL & zIPL. Source code - Broken down into a source file for the driver and interrupt, one for the header file and one for the debugger.

More information can be found at: http://gavingraham.com/geek-stuff-new-old/c64-retro-coding/

- Gavin (Gazza)
