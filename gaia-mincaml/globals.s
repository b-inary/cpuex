
.global n_objects
n_objects:
    .int    0

.global objects
objects:
    .space  240

.global screen
screen:
    .space  12

.global viewpoint
viewpoint:
    .space  12

.global light
light:
    .space  12

.global beam
beam:
    .float  255.0

.global and_net
and_net:
    .space  200

.global or_net
or_net:
    .int    0

.global solver_dist
solver_dist:
    .float  0.0

.global intsec_rectside
intsec_rectside:
    .int    0

.global tmin
tmin:
    .float  1000000000.0

.global intersection_point
intersection_point:
    .space  12

.global intersected_object_id
intersected_object_id:
    .int    0

.global nvector
nvector:
    .space  12

.global texture_color
texture_color:
    .space  12

.global diffuse_ray
diffuse_ray:
    .space  12

.global rgb
rgb:
    .space  12

.global image_size
image_size:
    .space  8

.global image_center
image_center:
    .space  8

.global scan_pitch
scan_pitch:
    .float  0.0

.global startp
startp:
    .space  12

.global startp_fast
startp_fast:
    .space  12

.global screenx_dir
screenx_dir:
    .space  12

.global screeny_dir
screeny_dir:
    .space  12

.global screenz_dir
screenz_dir:
    .space  12

.global ptrace_dirvec
ptrace_dirvec:
    .space  12

.global dirvecs
dirvecs:
    .space  20

.global light_dirvec
light_dirvec:
    .space  8

.global reflections
reflections:
    .space  720

.global n_reflections
n_reflections:
    .int    0

.global heap_ptr
heap_ptr:
    .int    _end_of_program

.global main
main:
    mov     r1, 1
    mov     r2, -1
    call    create_array
    mov     r2, 196
    mov     r3, and_net
L1:
    add     r4, r2, r3
    mov     [r4], r1
    sub     r2, r2, 4
    bge     r2, 0, L1
    mov     r2, r1
    mov     r1, 1
    call    create_array
    mov     [or_net], r1
    # write   r1, "\xaa"
    mov     r1, min_caml_main
    jr      r1
