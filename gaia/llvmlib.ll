
define i32 @read() nounwind {
  %1 = call i32 @getchar()
  ret i32 %1
}

define void @write(i32 %x) nounwind {
  %1 = call i32 @putchar(i32 %x)
  ret void
}

define i32 @ftoi(float %f) nounwind readonly {
  %1 = fadd fast float %f, 0.5
  %2 = call float @llvm.floor.f32(float %1)
  %3 = fptosi float %2 to i32
  ret i32 %3
}

declare i32 @getchar() nounwind
declare i32 @putchar(i32) nounwind
declare float @llvm.floor.f32(float) nounwind readonly

