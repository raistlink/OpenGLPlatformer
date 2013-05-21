;;; Copyright (c) 2013 by Álvaro Castro Castilla
;;; OpenGL 2.1 2d skeleton

(define screen-width 1280.0)
(define screen-height 752.0)
(define tile-height 25.0)
(define tile-width 25.0)
(define vertex-data-vector '#f32())


;;Level information is loaded from here.
(load "Leveldata.dat")



(define-structure player posx posy hstate vstate)
(define-structure enemy posx posy)
(define-structure world gamestate player enemy)


(define addTile (lambda (x y px py)
                  (set! vertex-data-vector 
                        (f32vector-append vertex-data-vector 
                                          (list->f32vector (list x y (* 0.25 px) (* 0.25 py)
                                                                 x (+ y tile-height) (* 0.25 px) (* 0.25 (+ py 1))
                                                                 (+ x tile-width) (+ y tile-height) (* 0.25 (+ px 1)) (* 0.25 (+ py 1))
                                                                 (+ x tile-width) y (* 0.25 (+ px 1)) (* 0.25 py)))))))


(define addBackground (lambda (px py)
                  (set! vertex-data-vector 
                        (f32vector-append vertex-data-vector 
                                          (list->f32vector (list 0.0 0.0 (* 0.25 px) (* 0.25 py)
                                                                 0.0 (+ 0.0 (- screen-height 5)) (* 0.25 px) (* 0.25 (+ py 1))
                                                                 (+ 0.0 (- screen-width 5)) (+ 0.0 (- screen-height 5)) (* 0.25 (+ px 1)) (* 0.25 (+ py 1))
                                                                 (+ 0.0 (- screen-width 5)) 0.0 (* 0.25 (+ px 1)) (* 0.25 py)))))))

;; Functions to get from one level to another, and spawn position data
(define levellist (cons level1 (cons level2 (cons level3 '()))))
(define level+ (cons 28 (cons 3 (cons 8 '()))))
(define level- (cons 3 (cons 28 (cons 3 '()))))


;; Funcion to get the level we are in.
(define (getlevel llist lcounter)
  (let loop ((rest llist) (counter 0))
    (if (eq? counter lcounter)
        (car rest)
        (loop (cdr rest) (+ counter 1)))))

;; Get the last element of a list.
(define (last nlist)
  (let loop ((rest nlist))
    (if (null? (cdr rest))
        (car rest)
        (loop (cdr rest)))))



(define vertex-shader #<<end-of-shader

#version 120
attribute vec2 position;
attribute vec2 texCoord;

varying vec2 colorCoord;

uniform mat4 perspectiveMatrix;

void main()
{
  colorCoord = texCoord;
  gl_Position = perspectiveMatrix * vec4(position, 0.0, 1.0);
}

end-of-shader
)

(define fragment-shader #<<end-of-shader
   
#version 120

varying vec2 colorCoord;
uniform sampler2D colorTexture;

void main()
{
  gl_FragColor = texture2D(colorTexture, colorCoord);
}

end-of-shader
)


(define (main)
  (let ((init-screen-width 1280)
        (init-screen-height 752)
        (screen-width* (alloc-int* 1))
        (screen-height* (alloc-int* 1)))
    (when (< (SDL_Init SDL_INIT_VIDEO) 0) report: (fusion:error "Couldn't initialize SDL!"))
    ;; SDL
    (let ((win (SDL_CreateWindow
                ""
                SDL_WINDOWPOS_CENTERED
                SDL_WINDOWPOS_CENTERED
                (cond-expand (mobile 0) (else init-screen-width))
                (cond-expand (mobile 0) (else init-screen-height))
                SDL_WINDOW_OPENGL)))
      (unless win (fusion:error "Unable to create render window" (SDL_GetError)))
      (SDL_GetWindowSize win screen-width* screen-height*)
      (let ((screen-width (*->int screen-width*))
            (screen-height (*->int screen-height*))
            (ctx (SDL_GL_CreateContext win)))
        (SDL_Log (string-append "SDL screen size: " (object->string screen-width) " x " (object->string screen-height)))
        ;; OpenGL
        (SDL_Log (string-append "OpenGL Version: " (*->string (glGetString GL_VERSION))))
        (SDL_Log "Using API OpenGL Version: 2.1 - GL Shading Language Version: 1.2")
        ;; Glew: initialize extensions
        (glewInit)
        ;; OpenGL viewport
        (glViewport 0 0 screen-width screen-height)
        (glScissor 0 0 screen-width screen-height)

        ;;Start sound mixer
        (unless (= 0 (Mix_OpenAudio 44100 MIX_DEFAULT_FORMAT 2 1024))
                (fusion:error (string-append "Unable to initialize sound system -- " (Mix_GetError)))) 
        
        ;; Generate programs, buffers, textures
        (let* ((perspective-matrix (matrix:* (make-translation-matrix -1.0 1.0 0.0)
                                             (matrix:* (make-scaling-matrix (/ 2.0 screen-width) (/ -2.0 screen-height) 1.0)
                                                       (make-identity-matrix))))
               (position-buffer-object-id* (alloc-GLuint* 1))
               (main-vao-id* (alloc-GLuint* 1))
               (surface-id* (alloc-GLuint* 1))
               (texture-id* (alloc-GLuint* 1))
               (texture-unit 0)
               (sampler-id* (alloc-GLuint* 1))

               
               (vertex-data (f32vector->GLfloat* vertex-data-vector))
               (shaders (list (fusion:create-shader GL_VERTEX_SHADER vertex-shader)
                              (fusion:create-shader GL_FRAGMENT_SHADER fragment-shader)))
               (shader-program (fusion:create-program shaders))
               
               (texture-image* (IMG_Load "assets/128x128-2.png"))
               
               ;;Background Music
               (background-music* (or (Mix_LoadMUS "assets/background.ogg")
                                      (fusion:error (string-append "Unable to load OGG music -- " (Mix_GetError))))) 
               ;;Sound effects
               (jump-sound* (or (Mix_LoadWAV "assets/Jump.wav")
                                 (fusion:error (string-append "Unable to load WAV chunk -- " (Mix_GetError)))))
               (death-sound* (or (Mix_LoadWAV "assets/Death.wav")
                                 (fusion:error (string-append "Unable to load WAV chunk -- " (Mix_GetError)))))
               (burnt-sound* (or (Mix_LoadWAV "assets/Fire.wav")
                                 (fusion:error (string-append "Unable to load WAV chunk -- " (Mix_GetError)))))

               )
          ;; Clean up shaders once the program has been compiled and linked
          (for-each glDeleteShader shaders)


          ;; Texture
          (glGenTextures 1 texture-id*)
          (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
          (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA
                        (SDL_Surface-w texture-image*) (SDL_Surface-h texture-image*)
                        0 GL_RGBA GL_UNSIGNED_BYTE
                        (SDL_Surface-pixels texture-image*))
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0)
          (glBindTexture GL_TEXTURE_2D 0)
          (SDL_FreeSurface texture-image*)
          (glEnable GL_BLEND)
          (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
                   

          ;; Uniforms
          (glUseProgram shader-program)
          (glUniformMatrix4fv (glGetUniformLocation shader-program "perspectiveMatrix")
                              1 GL_FALSE
                              (matrix->GLfloat*
                               (matrix:map exact->inexact
                                           perspective-matrix)))
          (glUniform1i (glGetUniformLocation shader-program "colorTexture") texture-unit)
          (glUseProgram 0)

          ;; Sampler
          (glGenSamplers 1 sampler-id*)
          (let ((sampler-id (*->GLuint sampler-id*)))
            (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
            (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
            (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
            (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST))
          
          ;; Vertex Array Object
          (glGenBuffers 1 position-buffer-object-id*)
          (let ((position-buffer-object-id (*->GLuint position-buffer-object-id*)))
            ;; Upload buffer
            (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
            (glBufferData GL_ARRAY_BUFFER
                          (* (f32vector-length vertex-data-vector) GLfloat-size)
                          vertex-data
                          GL_DYNAMIC_DRAW)
            ;; Create VAO
            (glGenVertexArrays 1 main-vao-id*)
            (glBindVertexArray (*->GLuint main-vao-id*))
            (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
            
            (let ((position-attr (glGetAttribLocation shader-program "position"))
                  (texture-coordinates-attr (glGetAttribLocation shader-program "texCoord")))
              (glEnableVertexAttribArray position-attr)
              (glVertexAttribPointer position-attr 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
              (glEnableVertexAttribArray texture-coordinates-attr)
              (glVertexAttribPointer texture-coordinates-attr 2
                                     GL_FLOAT GL_FALSE
                                     (* 4 GLfloat-size) (integer->void* (* 2 GLfloat-size))))
            
            (glBindBuffer GL_ARRAY_BUFFER 0)
            (glBindVertexArray 0)
            
            ;; Game loop
            (let ((event* (alloc-SDL_Event)))
              (call/cc
               (lambda (quit)
                 (let main-loop ((world (make-world 'splash-screen '() '()))
                                 (time (SDL_GetTicks))
                                 (jumpcounter 0) 
                                 (levelcounter 0) 
                                 (enemyX '(0)) 
                                 (enemyY '(0)) 
                                 (enemycounter 0)
                                 (playerAnim 0)
                                 (enemyAnim 0))
                   (let event-loop ()
                     (when (= 1 (SDL_PollEvent event*))
                           (let ((event-type (SDL_Event-type event*)))
                             (cond
                              ((= event-type SDL_KEYDOWN)
                               (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key down")
                               (let* ((kevt* (SDL_Event-key event*))
                                      (key (SDL_Keysym-sym
                                            (SDL_KeyboardEvent-keysym kevt*))))
                                 (cond ((= key SDLK_ESCAPE)
                                        (quit))
                                       ((= key SDLK_RETURN)
                                        (if (eq? (world-gamestate world) 'splash-screen)
                                            (set! world (make-world 'game-screen (make-player (* tile-width 2) (* tile-width 28) 'idle 'idle) (make-enemy (- 0 tile-width) (- 0 tile-height))))
                                            (if (not (eq? (world-gamestate world) 'game-screen))
                                                (set! world (make-world 'splash-screen '() '())))))
                                       ((= key SDLK_LEFT)
                                        (if (eq? (world-gamestate world) 'game-screen)
                                            (set! world (make-world (world-gamestate world) (make-player (player-posx (world-player world)) (player-posy (world-player world)) 'left (player-vstate (world-player world))) (world-enemy world)))))
                                       ((= key SDLK_RIGHT)
                                        (if (eq? (world-gamestate world) 'game-screen)
                                            (set! world (make-world (world-gamestate world) (make-player (player-posx (world-player world)) (player-posy (world-player world)) 'right (player-vstate (world-player world))) (world-enemy world)))))
                                       ((= key SDLK_UP)
                                        (if (and (eq? (world-gamestate world) 'game-screen) (eq? (player-vstate (world-player world)) 'idle))
                                            (begin
                                              (set! world (make-world (world-gamestate world) (make-player (player-posx (world-player world)) (player-posy (world-player world)) (player-hstate (world-player world)) 'jump) (world-enemy world)))
                                              (pp "Saltando")
                                              (Mix_PlayChannel 2 jump-sound* 0))))
                                       (else
                                        (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))

                              ((= event-type SDL_KEYUP)
                               (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key up")
                               (let* ((kevt* (SDL_Event-key event*))
                                      (key (SDL_Keysym-sym
                                            (SDL_KeyboardEvent-keysym kevt*))))
                                 (cond  ((= key SDLK_LEFT)
                                         (if (and (eq? (world-gamestate world) 'game-screen) (eq? (player-hstate (world-player world)) 'left))
                                             (set! world (make-world (world-gamestate world) (make-player (player-posx (world-player world)) (player-posy (world-player world)) 'idle (player-vstate (world-player world))) (world-enemy world)))))
                                        ((= key SDLK_RIGHT)
                                         (if (and (eq? (world-gamestate world) 'game-screen) (eq? (player-hstate (world-player world)) 'right))
                                             (set! world (make-world (world-gamestate world) (make-player (player-posx (world-player world)) (player-posy (world-player world)) 'idle (player-vstate (world-player world))) (world-enemy world)))))
                                        ((= key SDLK_UP)
                                         (if (and (eq? (world-gamestate world) 'game-screen) (eq? (player-vstate (world-player world)) 'jump))
                                             (set! world (make-world (world-gamestate world) (make-player (player-posx (world-player world)) (player-posy (world-player world)) (player-hstate (world-player world)) 'falling) (world-enemy world))) ))
                                        (else
                                         (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
                              (else #f)))
                           (event-loop)))
                   
                   ;; -- Game logic --

                   ;; Limpiamos el buffer.
                   (set! vertex-data-vector '#f32())

                   ;; Empezamos la musica de fondo.

                   (if (= 0 (Mix_PlayingMusic))
                       (Mix_PlayMusic background-music* -1))

                   (case (world-gamestate world)
                     ((splash-screen)
                      ;; Drawing the menu image
                      
                      (addBackground 2.0 0.0))
                     

                     ((game-screen)
                      
                      ;; Drawing the background
                      (addBackground 3.0 0.0)
                      
                      ;; Drawing the tiles.
                      (let loop ((rest (getlevel levellist levelcounter)) (counterX 0) (counterY 0))
                        (if (and (eq? counterX 50) (eq? counterY 29))
                            (addTile (* counterX tile-width) (* counterY tile-height) 1.0 1.0)
                            (begin
                              (if (eq? (vector-ref (vector-ref rest counterY) counterX) 1)
                                  (addTile (exact->inexact (* counterX tile-width)) (exact->inexact (* counterY tile-height)) 1.0 1.0))
                              (if (eq? (vector-ref (vector-ref rest counterY) counterX) 4)
                                  (addTile (exact->inexact (* counterX tile-width)) (exact->inexact (* counterY tile-height)) 0.0 0.0))
                              (if (eq? counterX 50)
                                  (loop rest (- counterX 50) (+ counterY 1))
                                  (loop rest (+ counterX 1) counterY)))))

                      ;; Drawing the player.

                      (if (eq? playerAnim 0)
                          (addTile (exact->inexact (player-posx (world-player world))) (exact->inexact (player-posy (world-player  world))) 0.0 1.0)
                          (addTile (exact->inexact (player-posx (world-player world))) (exact->inexact (player-posy (world-player  world))) 0.0 2.0))
                      (if (eq? (player-hstate (world-player world)) 'right)
                          (set! playerAnim 0))
                      (if (eq? (player-hstate (world-player world)) 'left)
                          (set! playerAnim 1))


                      ;; Drawing the enemy

                      (if (eq? enemyAnim 0)
                          (addTile (exact->inexact (enemy-posx (world-enemy world))) (exact->inexact (enemy-posy (world-enemy  world))) 1.0 2.0)
                          (addTile (exact->inexact (enemy-posx (world-enemy world))) (exact->inexact (enemy-posy (world-enemy  world))) 1.0 0.0))


                      ;;Player Movement Calculation
          
                      ;;Going Left
                      (if (eq? (player-hstate (world-player world)) 'left)
                          (begin
                            (if (or (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (player-posy (world-player world)) tile-height)))) 
                                                     (inexact->exact (floor (/ (- (player-posx (world-player world)) (/ tile-width 25)) tile-width)))) 1)
                                    (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (+ (- tile-height (/ tile-height 16.666)) (player-posy (world-player world))) tile-height)))) 
                                                     (inexact->exact (floor (/ (- (player-posx (world-player world)) (/ tile-width 25)) tile-width)))) 1) )
                                (player-posx-set! (world-player world) (+ (player-posx (world-player world)) 0))
                                (player-posx-set! (world-player world) (- (player-posx (world-player world)) (/ tile-width 10))))))
                      
                      ;;Going Right
                      (if (eq? (player-hstate (world-player world)) 'right)
                          (begin
                            (if (or (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (player-posy (world-player world)) tile-height)))) 
                                                     (inexact->exact (floor  (/  (+ (+ tile-width (/ tile-width 10)) (player-posx (world-player world))) tile-width)))) 1)
                                    (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (+ (- tile-height (/ tile-height 16.666)) (player-posy (world-player world))) tile-height)))) 
                                                     (inexact->exact (floor  (/  (+ (+ tile-width (/ tile-width 10)) (player-posx (world-player world))) tile-width)))) 1))
                                (player-posx-set! (world-player world) (- (player-posx (world-player world)) 0))
                                (player-posx-set! (world-player world) (+ (player-posx (world-player world)) (/ tile-width 10))))))
                      
                      
                      ;;Falling
                      (if (or (eq? (player-vstate (world-player world)) 'idle) (eq? (player-vstate (world-player world)) 'falling))
                          (begin
                            (if (or (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (+ (inexact->exact (floor (/ (player-posy (world-player world)) tile-height))) 1))
                                                     (inexact->exact (floor (/ (player-posx (world-player world)) tile-width)))) 1)
                                    (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (+ (inexact->exact (floor (/ (player-posy (world-player world)) tile-height))) 1))
                                                     (inexact->exact (floor (/ (+ tile-width (player-posx (world-player world))) tile-width)))) 1))
                                (begin
                                  (player-vstate-set! (world-player world) 'idle)
                                  (set! jumpcounter 0))
                                (begin
                                  (player-posy-set! (world-player world) (+ (player-posy (world-player world)) (/ tile-height 5)))
                                  (player-vstate-set! (world-player world) 'falling)))))
                      
                      ;;Jumping
                      (if (eq? (player-vstate (world-player world)) 'jump)
                          (begin
                            (if (or (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (player-posy (world-player world)) tile-height))))
                                                     (inexact->exact (floor (/ ( player-posx (world-player world)) tile-width)))) 1)
                                    (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (player-posy (world-player world)) tile-height))))
                                                     (inexact->exact (floor (/ (+ tile-width (player-posx (world-player world))) tile-width)))) 1) 
                                    )
                                (player-vstate-set! (world-player world) 'falling)
                                (if (> jumpcounter (* tile-width 6))
                                    (begin
                                      (player-vstate-set! (world-player world) 'falling)
                                      (set! jumpcounter 0))
                                    (begin
                                      (player-posy-set! (world-player world) (- (player-posy (world-player world)) (/ tile-width 5)))
                                      (set! jumpcounter (+ jumpcounter (/ tile-width 5))))))))
                      
                      
                      ;;Enemy Calculations
                      
                      ;;Spawn counter
                      (if (eq? enemycounter 0)
                          (set! enemycounter time))
                      
                      ;;Enemy position updating
                      (if (> (- time enemycounter) 1500)
                          (begin
                            ;; Drawing the enemy.
                            (if (not (and (eq? (player-posx (world-player world)) (last enemyX))
                                          (eq? (player-posy (world-player world)) (last enemyY))))
                                
                                (if (> (car enemyX) (car (cdr enemyX)))
                                    (set! enemyAnim 0)
                                    (set! enemyAnim 1)))
                            (enemy-posx-set! (world-enemy world) (car enemyX))
                            (enemy-posy-set! (world-enemy world) (car enemyY))
                            (if (not (null? (cdr enemyX)))
                                (begin (set! enemyX (cdr enemyX))
                                       (set! enemyY (cdr enemyY))))))
                      
                      ;;Enemy position list updating
                      (if (not (and (eq? (player-posx (world-player world)) (last enemyX))
                                    (eq? (player-posy (world-player world)) (last enemyY))
                                    ))
                          (begin
                            (set! enemyX (append enemyX (list (player-posx (world-player world)))))
                            (set! enemyY (append enemyY (list (player-posy (world-player world)))))))
                      
                      ;;Enemy Collision detection.
                      
                      (if (and (< (abs (- (+ (player-posx (world-player world)) (/ tile-width 2)) (+ (enemy-posx (world-enemy world)) (/ tile-height 2)))) (/ tile-width 2))
                               (< (abs (- (+ (player-posy (world-player world)) (/ tile-width 2)) (+ (enemy-posy (world-enemy world)) (/ tile-height 2)))) (/ tile-width 2)))
                          (begin
                            (world-gamestate-set! world 'death-screen)
                            (enemy-posx-set! (world-enemy world) (- 0 tile-width))
                            (enemy-posy-set! (world-enemy world) (- 0 tile-width))
                            (set! enemyX '(0))
                            (set! enemyY '(0))
                            (set! enemycounter 0)
                            (set! levelcounter 0)
                            (Mix_PlayChannel 1 death-sound* 0)))
                      
                      
                      
                      ;;Game calculations
                      
                      ;; Obstacle collision calculation
                      
                      ;;Collision on the left
                      (if (eq? (player-hstate (world-player world)) 'left)
                          (begin
                            (if (or (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (player-posy (world-player world)) tile-height)))) 
                                                     (inexact->exact (floor (/ (- (player-posx (world-player world)) (/ tile-width 25)) tile-width)))) 4)
                                    (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (+ (- tile-height (/ tile-height 16.666)) (player-posy (world-player world))) tile-height)))) 
                                                     (inexact->exact (floor (/ (- (player-posx (world-player world)) (/ tile-width 25)) tile-width)))) 4) )
                                (begin
                                  (world-gamestate-set! world 'death-screen)
                                  (enemy-posx-set! (world-enemy world) (- 0 tile-width))
                                  (enemy-posy-set! (world-enemy world) (- 0 tile-width))
                                  (set! enemyX '(0))
                                  (set! enemyY '(0))
                                  (set! enemycounter 0)
                                  (set! levelcounter 0)
                                  (Mix_PlayChannel 1 burnt-sound* 0)))))
                      
                      ;;Collision on the right
                      (if (eq? (player-hstate (world-player world)) 'right)
                          (begin
                            (if (or (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (player-posy (world-player world)) tile-height)))) 
                                                     (inexact->exact (floor  (/  (+ (+ tile-width (/ tile-width 10)) (player-posx (world-player world))) tile-width)))) 4)
                                    (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (+ (- tile-height (/ tile-height 16.666)) (player-posy (world-player world))) tile-height)))) 
                                                     (inexact->exact (floor  (/  (+ (+ tile-width (/ tile-width 10)) (player-posx (world-player world))) tile-width)))) 4))
                                (begin
                                  (world-gamestate-set! world 'death-screen)
                                  (enemy-posx-set! (world-enemy world) (- 0 tile-width))
                                  (enemy-posy-set! (world-enemy world) (- 0 tile-width))
                                  (set! enemyX '(0))
                                  (set! enemyY '(0))
                                  (set! enemycounter 0)
                                  (set! levelcounter 0)
                                  (Mix_PlayChannel 1 burnt-sound* 0)))))
                      
                      
                      ;;Collision on bottom
                      (if (or (eq? (player-vstate (world-player world)) 'idle) (eq? (player-vstate (world-player world)) 'falling))
                          (begin
                            (if (or (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (+ (inexact->exact (floor (/ (player-posy (world-player world)) tile-height))) 1))
                                                     (inexact->exact (floor (/ (player-posx (world-player world)) tile-width)))) 4)
                                    (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (+ (inexact->exact (floor (/ (player-posy (world-player world)) tile-height))) 1))
                                                     (inexact->exact (floor (/ (+ tile-width (player-posx (world-player world))) tile-width)))) 4))
                                (begin
                                  (world-gamestate-set! world 'death-screen)
                                  (enemy-posx-set! (world-enemy world) (- 0 tile-width))
                                  (enemy-posy-set! (world-enemy world) (- 0 tile-width))
                                  (set! enemyX '(0))
                                  (set! enemyY '(0))
                                  (set! enemycounter 0)
                                  (set! levelcounter 0)
                                  (Mix_PlayChannel 1 burnt-sound* 0)))))
                      
                      ;;Collision on top
                      (if (eq? (player-vstate (world-player world)) 'jump)
                          (begin
                            (if (or (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (player-posy (world-player world)) tile-height))))
                                                     (inexact->exact (floor (/ ( player-posx (world-player world)) tile-width)))) 4)
                                    (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (player-posy (world-player world)) tile-height))))
                                                     (inexact->exact (floor (/ (+ tile-width (player-posx (world-player world))) tile-width)))) 4) 
                                    )
                                (begin
                                  (world-gamestate-set! world 'death-screen)
                                  (enemy-posx-set! (world-enemy world) (- 0 tile-width))
                                  (enemy-posy-set! (world-enemy world) (- 0 tile-width))
                                  (set! enemyX '(0))
                                  (set! enemyY '(0))
                                  (set! enemycounter 0)
                                  (set! levelcounter 0)
                                  (Mix_PlayChannel 1 fire-sound* 0)))))
                      
                      
                      ;; Level Complete
                      (if (eq? (player-hstate (world-player world)) 'right)
                          (begin
                            (if (or (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (player-posy (world-player world)) tile-height)))) 
                                                     (inexact->exact (floor  (/  (+ (+ tile-width (/ tile-width 10)) (player-posx (world-player world))) tile-width)))) 2)
                                    (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (+ (- tile-height (/ tile-height 16.666)) (player-posy (world-player world))) tile-height)))) 
                                                     (inexact->exact (floor  (/  (+ (+ tile-width (/ tile-width 10)) (player-posx (world-player world))) tile-width)))) 2))
                                (begin
                                  (set! levelcounter (+ levelcounter 1))
                                  (player-posy-set! (world-player world) (* tile-width (getlevel level+ levelcounter)))
                                  (player-posx-set! (world-player world) (* tile-width 2))
                                  (enemy-posx-set! (world-enemy world) (- 0 tile-width))
                                  (enemy-posy-set! (world-enemy world) (- 0 tile-width))
                                  (set! enemyX '(0))
                                  (set! enemyY '(0))
                                  (set! enemycounter 0)))))
                      
                      ;; Level Back
                      (if (eq? (player-hstate (world-player world)) 'left)
                          (begin
                            (if (or (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (player-posy (world-player world)) tile-height)))) 
                                                     (inexact->exact (floor (/ (- (player-posx (world-player world)) (/ tile-width 25)) tile-width)))) 3)
                                    (eq? (vector-ref (vector-ref (getlevel levellist levelcounter) (inexact->exact (floor (/ (+ (- tile-height (/ tile-height 16.666)) (player-posy (world-player world))) tile-height)))) 
                                                     (inexact->exact (floor (/ (- (player-posx (world-player world)) (/ tile-width 25)) tile-width)))) 3) )
                                (begin
                                  (set! levelcounter (- levelcounter 1))
                                  (player-posy-set! (world-player world) (* tile-width (getlevel level- levelcounter)))
                                  (player-posx-set! (world-player world) (* tile-width 49))
                                  (enemy-posx-set! (world-enemy world) (- 0 tile-width))
                                  (enemy-posy-set! (world-enemy world) (- 0 tile-width))
                                  (set! enemyX '(0))
                                  (set! enemyY '(0))
                                  (set! enemycounter 0))))))


                     ((death-screen)
                      
                      ;;Drawing the death background
                      (addBackground 2.0 1.0)
                      ))

                   ;; -- Draw -
                   (glClearColor 0.0 0.0 0.0 0.0)
                   (glClear GL_COLOR_BUFFER_BIT)
                   (glActiveTexture (+ GL_TEXTURE0 texture-unit))
                   (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
                   (glBindSampler texture-unit (*->GLuint sampler-id*))

                   ;; Begin VAO
                   (glBindVertexArray (*->GLuint main-vao-id*))
                   ;; Update vertex data buffer
                   (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
                   (glBufferData GL_ARRAY_BUFFER
                          (* (f32vector-length vertex-data-vector) GLfloat-size)
                          (f32vector->GLfloat* vertex-data-vector)
                          GL_DYNAMIC_DRAW)
                   
                   (glUseProgram shader-program)
                   (glDrawArrays GL_QUADS 0 (/ (f32vector-length vertex-data-vector) 4))
                   (glUseProgram 0)
                   (glBindVertexArray 0)
                   ;; End VAO
                   
                   (SDL_GL_SwapWindow win)
                   (main-loop world (SDL_GetTicks) jumpcounter levelcounter enemyX enemyY enemycounter playerAnim enemyAnim))))
              (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
              (SDL_GL_DeleteContext ctx)
              (SDL_DestroyWindow win)
              (SDL_Quit)))))))
  (##gc))

