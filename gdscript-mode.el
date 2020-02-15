;;; gdscript-mode.el --- Major mode to add support for Godot's GDScript programming language. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest, Free Software Foundation, Inc.

;; Author: Nathan Lovato <nathan@gdquest.com>, Fabián E. Gallina <fgallina@gnu.org>
;; URL: https://github.com/GDQuest/emacs-gdscript-mode/
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.0"))
;; Maintainer: nathan@gdquest.com
;; Created: Jan 2020
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for the Godot GDScript programming language in Emacs.

;;; Code:

(add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-mode))
(add-to-list 'auto-mode-alist '("\\.tscn\\'" . conf-toml-mode))
(add-to-list 'auto-mode-alist '("\\.tres\\'" . conf-toml-mode))

(defgroup gdscript nil
  "GDScript language support for Emacs."
  :group 'languages
  :version "26"
  :link '(emacs-commentary-link "gdscript"))

;; gdscript-rx is a copy of Emacs 27's rx module, to ensure compatibility with
;; Emacs 26
(if (version< emacs-version "27")
    (require 'gdscript-rx))

(defvar gdscript-mode-map (let ((map (make-sparse-keymap)))
                            ;; Movement
                            (define-key map [remap backward-sentence] 'gdscript-nav-backward-block)
                            (define-key map [remap forward-sentence] 'gdscript-nav-forward-block)
                            (define-key map [remap backward-up-list] 'gdscript-nav-backward-up-list)
                            (define-key map [remap mark-defun] 'gdscript-mark-defun)
                            (define-key map "\C-c\C-j" 'imenu)
                            ;; Indent specific
                            (define-key map "\177" 'gdscript-indent-dedent-line-backspace)
                            (define-key map (kbd "<backtab>") 'gdscript-indent-dedent-line)
                            map)
  "Keymap for `gdscript-mode'.")

(defcustom gdscript-use-type-hints t
  "If t, inserted snippets contain type hints"
  :group 'gdscript
  :type 'boolean)


;; Lists of keywords in the language
(defvar gdscript-keywords '("if" "elif" "else" "for" "do" "while" "match"
                            "switch" "case" "break" "continue" "pass"
                            "return" "class" "class_name" "extends" "is"
                            "self" "tool" "signal" "func" "static" "const"
                            "enum" "var" "onready" "export" "setget" "breakpoint"
                            "preload" "yield" "assert" "remote" "master"
                            "slave" "sync"))

(defvar gdscript-built-in-constants '("PI" "TAU" "INF" "NAN"))

;; Only contains types that are not classes and that the Godot editor highlights
;; like built-in keywords
(defvar gdscript-built-in-types '("null" "void" "bool" "int" "float"))

(defvar gdscript-built-in-functions '("sin" "cos" "tan" "sinh" "cosh" "tanh" "asin"
                                      "acos" "atan" "atan2" "sqrt" "fmod" "fposmod"
                                      "floor" "ceil" "round" "abs" "sign" "pow"
                                      "log" "exp" "is_nan" "is_inf" "ease" "decimals"
                                      "stepify" "lerp" "dectime" "randomize" "randi"
                                      "randf" "rand_range" "seed" "rand_seed" "deg2rad"
                                      "rad2deg" "linear2db" "db2linear" "max" "min"
                                      "clamp" "nearest_po2" "weakref" "funcref"
                                      "convert" "typeof" "type_exists" "char" "str"
                                      "print" "printt" "prints" "printerr" "printraw"
                                      "var2str" "str2var" "var2bytes" "bytes2var"
                                      "range" "load" "inst2dict" "dict2inst" "hash"
                                      "Color8" "print_stack" "instance_from_id"
                                      "preload" "yield" "assert" "name"))

;; Contains all engine classes and node types, including vectors, transforms, etc.
(defvar gdscript-built-in-classes '("AABB" "AcceptDialog" "AnimatedSprite3D"
                                    "AnimatedSprite" "AnimatedTexture" "AnimationNodeAdd2"
                                    "AnimationNodeAdd3" "AnimationNodeAnimation"
                                    "AnimationNodeBlend2" "AnimationNodeBlend3"
                                    "AnimationNodeBlendSpace1D" "AnimationNodeBlendSpace2D"
                                    "AnimationNodeBlendTree" "AnimationNodeOneShot"
                                    "AnimationNodeOutput" "AnimationNodeStateMachinePlayback"
                                    "AnimationNodeStateMachineTransition" "AnimationNodeStateMachine"
                                    "AnimationNodeTimeScale" "AnimationNodeTimeSeek"
                                    "AnimationNodeTransition" "AnimationNode"
                                    "AnimationPlayer" "AnimationRootNode" "AnimationTrackEditPlugin"
                                    "AnimationTreePlayer" "AnimationTree" "Animation"
                                    "Area2D" "Area" "ArrayMesh" "Array" "ARVRAnchor"
                                    "ARVRCamera" "ARVRController" "ARVRInterface"
                                    "ARVROrigin" "ARVRPositionalTracker" "ARVRServer"
                                    "AStar2D" "AStar" "AtlasTexture" "AudioBusLayout"
                                    "AudioEffectAmplify" "AudioEffectBandLimitFilter"
                                    "AudioEffectBandPassFilter" "AudioEffectChorus"
                                    "AudioEffectCompressor" "AudioEffectDelay"
                                    "AudioEffectDistortion" "AudioEffectEQ10"
                                    "AudioEffectEQ21" "AudioEffectEQ6" "AudioEffectEQ"
                                    "AudioEffectFilter" "AudioEffectHighPassFilter"
                                    "AudioEffectHighShelfFilter" "AudioEffectInstance"
                                    "AudioEffectLimiter" "AudioEffectLowPassFilter"
                                    "AudioEffectLowShelfFilter" "AudioEffectNotchFilter"
                                    "AudioEffectPanner" "AudioEffectPhaser" "AudioEffectPitchShift"
                                    "AudioEffectRecord" "AudioEffectReverb" "AudioEffectSpectrumAnalyzerInstance"
                                    "AudioEffectSpectrumAnalyzer" "AudioEffectStereoEnhance"
                                    "AudioEffect" "AudioServer" "AudioStreamGeneratorPlayback"
                                    "AudioStreamGenerator" "AudioStreamMicrophone"
                                    "AudioStreamPlaybackResampled" "AudioStreamPlayback"
                                    "AudioStreamPlayer2D" "AudioStreamPlayer3D"
                                    "AudioStreamPlayer" "AudioStreamRandomPitch"
                                    "AudioStreamSample" "AudioStream" "BackBufferCopy"
                                    "BakedLightmapData" "BakedLightmap" "BaseButton"
                                    "Basis" "BitmapFont" "BitMap" "Bone2D" "BoneAttachment"
                                    "bool" "BoxContainer" "BoxShape" "ButtonGroup"
                                    "Button" "Camera2D" "CameraFeed" "CameraServer"
                                    "CameraTexture" "Camera" "CanvasItemMaterial"
                                    "CanvasItem" "CanvasLayer" "CanvasModulate"
                                    "CapsuleMesh" "CapsuleShape2D" "CapsuleShape"
                                    "CenterContainer" "CharFXTransform" "CheckBox"
                                    "CheckButton" "CircleShape2D" "ClassDB" "ClippedCamera"
                                    "CollisionObject2D" "CollisionObject" "CollisionPolygon2D"
                                    "CollisionPolygon" "CollisionShape2D" "CollisionShape"
                                    "ColorPickerButton" "ColorPicker" "ColorRect"
                                    "Color" "ConcavePolygonShape2D" "ConcavePolygonShape"
                                    "ConeTwistJoint" "ConfigFile" "ConfirmationDialog"
                                    "Container" "Control" "ConvexPolygonShape2D"
                                    "ConvexPolygonShape" "CPUParticles2D" "CPUParticles"
                                    "CryptoKey" "Crypto" "CubeMap" "CubeMesh"
                                    "Curve2D" "Curve3D" "CurveTexture" "Curve"
                                    "CylinderMesh" "CylinderShape" "DampedSpringJoint2D"
                                    "Dictionary" "DirectionalLight" "Directory"
                                    "DynamicFontData" "DynamicFont" "EditorExportPlugin"
                                    "EditorFeatureProfile" "EditorFileDialog"
                                    "EditorFileSystemDirectory" "EditorFileSystem"
                                    "EditorImportPlugin" "EditorInspectorPlugin"
                                    "EditorInspector" "EditorInterface" "EditorNavigationMeshGenerator"
                                    "EditorPlugin" "EditorProperty" "EditorResourceConversionPlugin"
                                    "EditorResourcePreviewGenerator" "EditorResourcePreview"
                                    "EditorSceneImporterAssimp" "EditorSceneImporter"
                                    "EditorScenePostImport" "EditorScript" "EditorSelection"
                                    "EditorSettings" "EditorSpatialGizmoPlugin"
                                    "EditorSpatialGizmo" "EditorSpinSlider" "EditorVCSInterface"
                                    "EncodedObjectAsID" "Engine" "Environment"
                                    "Expression" "FileDialog" "File" "float" "Font"
                                    "FuncRef" "Generic6DOFJoint" "GeometryInstance"
                                    "Geometry" "GIProbeData" "GIProbe" "@GlobalScope"
                                    "GradientTexture" "Gradient" "GraphEdit" "GraphNode"
                                    "GridContainer" "GrooveJoint2D" "HashingContext"
                                    "HBoxContainer" "HeightMapShape" "HingeJoint"
                                    "HScrollBar" "HSeparator" "HSlider" "HSplitContainer"
                                    "HTTPClient" "HTTPRequest" "ImageTexture"
                                    "Image" "ImmediateGeometry" "InputDefault"
                                    "InputEventAction" "InputEventGesture" "InputEventJoypadButton"
                                    "InputEventJoypadMotion" "InputEventKey" "InputEventMagnifyGesture"
                                    "InputEventMIDI" "InputEventMouseButton" "InputEventMouseMotion"
                                    "InputEventMouse" "InputEventPanGesture" "InputEventScreenDrag"
                                    "InputEventScreenTouch" "InputEventWithModifiers"
                                    "InputEvent" "InputMap" "Input" "InstancePlaceholder"
                                    "InterpolatedCamera" "int" "IP_Unix" "IP"
                                    "ItemList" "JavaScript" "Joint2D" "Joint"
                                    "JSONParseResult" "JSONRPC" "JSON" "KinematicBody2D"
                                    "KinematicBody" "KinematicCollision2D" "KinematicCollision"
                                    "Label" "LargeTexture" "Light2D" "LightOccluder2D"
                                    "Light" "Line2D" "LineEdit" "LineShape2D"
                                    "LinkButton" "Listener" "MainLoop" "MarginContainer"
                                    "Marshalls" "Material" "MenuButton" "MeshDataTool"
                                    "MeshInstance2D" "MeshInstance" "MeshLibrary"
                                    "MeshTexture" "Mesh" "MultiMeshInstance2D"
                                    "MultiMeshInstance" "MultiMesh" "MultiplayerAPI"
                                    "Mutex" "Navigation2D" "NavigationMeshInstance"
                                    "NavigationMesh" "NavigationPolygonInstance"
                                    "NavigationPolygon" "Navigation" "NetworkedMultiplayerPeer"
                                    "Nil" "NinePatchRect" "Node2D" "NodePath"
                                    "Node" "Object" "OccluderPolygon2D" "OmniLight"
                                    "OptionButton" "OS" "PackedDataContainerRef"
                                    "PackedDataContainer" "PackedScene" "PacketPeerStream"
                                    "PacketPeerUDP" "PacketPeer" "PanelContainer"
                                    "Panel" "PanoramaSky" "ParallaxBackground"
                                    "ParallaxLayer" "Particles2D" "ParticlesMaterial"
                                    "Particles" "Path2D" "PathFollow2D" "PathFollow"
                                    "Path" "PCKPacker" "Performance" "PHashTranslation"
                                    "PhysicalBone" "Physics2DDirectBodyStateSW"
                                    "Physics2DDirectBodyState" "Physics2DDirectSpaceState"
                                    "Physics2DServerSW" "Physics2DServer" "Physics2DShapeQueryParameters"
                                    "Physics2DShapeQueryResult" "Physics2DTestMotionResult"
                                    "PhysicsBody2D" "PhysicsBody" "PhysicsDirectBodyState"
                                    "PhysicsDirectSpaceState" "PhysicsMaterial"
                                    "PhysicsServer" "PhysicsShapeQueryParameters"
                                    "PhysicsShapeQueryResult" "PinJoint2D" "PinJoint"
                                    "PlaneMesh" "PlaneShape" "Plane" "PointMesh"
                                    "Polygon2D" "PolygonPathFinder" "PoolByteArray"
                                    "PoolColorArray" "PoolIntArray" "PoolRealArray"
                                    "PoolStringArray" "PoolVector2Array" "PoolVector3Array"
                                    "PopupDialog" "PopupMenu" "PopupPanel" "Popup"
                                    "Position2D" "Position3D" "PrimitiveMesh"
                                    "PrismMesh" "ProceduralSky" "ProgressBar"
                                    "ProjectSettings" "ProximityGroup" "ProxyTexture"
                                    "QuadMesh" "Quat" "RandomNumberGenerator"
                                    "Range" "RayCast2D" "RayCast" "RayShape2D"
                                    "RayShape" "Rect2" "RectangleShape2D" "ReferenceRect"
                                    "Reference" "ReflectionProbe" "RemoteTransform2D"
                                    "RemoteTransform" "ResourceFormatLoaderCrypto"
                                    "ResourceFormatLoader" "ResourceFormatSaverCrypto"
                                    "ResourceFormatSaver" "ResourceImporter" "ResourceInteractiveLoader"
                                    "ResourceLoader" "ResourcePreloader" "ResourceSaver"
                                    "Resource" "RichTextEffect" "RichTextLabel"
                                    "RID" "RigidBody2D" "RigidBody" "RootMotionView"
                                    "SceneState" "SceneTreeTimer" "SceneTree"
                                    "ScriptCreateDialog" "ScriptEditor" "Script"
                                    "ScrollBar" "ScrollContainer" "SegmentShape2D"
                                    "Semaphore" "Separator" "ShaderMaterial" "Shader"
                                    "Shape2D" "Shape" "ShortCut" "Skeleton2D"
                                    "SkeletonIK" "Skeleton" "SkinReference" "Skin"
                                    "Sky" "SliderJoint" "Slider" "SoftBody" "SpatialGizmo"
                                    "SpatialMaterial" "SpatialVelocityTracker"
                                    "Spatial" "SphereMesh" "SphereShape" "SpinBox"
                                    "SplitContainer" "SpotLight" "SpringArm" "Sprite3D"
                                    "SpriteBase3D" "SpriteFrames" "Sprite" "StaticBody2D"
                                    "StaticBody" "StreamPeerBuffer" "StreamPeerSSL"
                                    "StreamPeerTCP" "StreamPeer" "StreamTexture"
                                    "String" "StyleBoxEmpty" "StyleBoxFlat" "StyleBoxLine"
                                    "StyleBoxTexture" "StyleBox" "SurfaceTool"
                                    "TabContainer" "Tabs" "TCP_Server" "TextEdit"
                                    "TextFile" "Texture3D" "TextureArray" "TextureButton"
                                    "TextureLayered" "TextureProgress" "TextureRect"
                                    "Texture" "Theme" "Thread" "TileMap" "TileSet"
                                    "Timer" "ToolButton" "TouchScreenButton" "Transform2D"
                                    "Transform" "TranslationServer" "Translation"
                                    "TreeItem" "Tree" "TriangleMesh" "Tween" "UndoRedo"
                                    "Variant" "VBoxContainer" "Vector2" "Vector3"
                                    "VehicleBody" "VehicleWheel" "VideoPlayer"
                                    "VideoStream" "ViewportContainer" "ViewportTexture"
                                    "Viewport" "VisibilityEnabler2D" "VisibilityEnabler"
                                    "VisibilityNotifier2D" "VisibilityNotifier"
                                    "VisualInstance" "VisualServer" "VisualShaderNodeBooleanConstant"
                                    "VisualShaderNodeBooleanUniform" "VisualShaderNodeColorConstant"
                                    "VisualShaderNodeColorFunc" "VisualShaderNodeColorOp"
                                    "VisualShaderNodeColorUniform" "VisualShaderNodeCompare"
                                    "VisualShaderNodeCubeMapUniform" "VisualShaderNodeCubeMap"
                                    "VisualShaderNodeCustom" "VisualShaderNodeDeterminant"
                                    "VisualShaderNodeDotProduct" "VisualShaderNodeExpression"
                                    "VisualShaderNodeFaceForward" "VisualShaderNodeFresnel"
                                    "VisualShaderNodeGlobalExpression" "VisualShaderNodeGroupBase"
                                    "VisualShaderNodeIf" "VisualShaderNodeInput"
                                    "VisualShaderNodeIs" "VisualShaderNodeOuterProduct"
                                    "VisualShaderNodeOutput" "VisualShaderNodeScalarClamp"
                                    "VisualShaderNodeScalarConstant" "VisualShaderNodeScalarDerivativeFunc"
                                    "VisualShaderNodeScalarFunc" "VisualShaderNodeScalarInterp"
                                    "VisualShaderNodeScalarOp" "VisualShaderNodeScalarSmoothStep"
                                    "VisualShaderNodeScalarSwitch" "VisualShaderNodeScalarUniform"
                                    "VisualShaderNodeSwitch" "VisualShaderNodeTextureUniformTriplanar"
                                    "VisualShaderNodeTextureUniform" "VisualShaderNodeTexture"
                                    "VisualShaderNodeTransformCompose" "VisualShaderNodeTransformConstant"
                                    "VisualShaderNodeTransformDecompose" "VisualShaderNodeTransformFunc"
                                    "VisualShaderNodeTransformMult" "VisualShaderNodeTransformUniform"
                                    "VisualShaderNodeTransformVecMult" "VisualShaderNodeUniform"
                                    "VisualShaderNodeVec3Constant" "VisualShaderNodeVec3Uniform"
                                    "VisualShaderNodeVectorClamp" "VisualShaderNodeVectorCompose"
                                    "VisualShaderNodeVectorDecompose" "VisualShaderNodeVectorDerivativeFunc"
                                    "VisualShaderNodeVectorDistance" "VisualShaderNodeVectorFunc"
                                    "VisualShaderNodeVectorInterp" "VisualShaderNodeVectorLen"
                                    "VisualShaderNodeVectorOp" "VisualShaderNodeVectorRefract"
                                    "VisualShaderNodeVectorScalarMix" "VisualShaderNodeVectorScalarSmoothStep"
                                    "VisualShaderNodeVectorScalarStep" "VisualShaderNodeVectorSmoothStep"
                                    "VisualShaderNode" "VisualShader" "VScrollBar"
                                    "VSeparator" "VSlider" "VSplitContainer" "WeakRef"
                                    "WindowDialog" "World2D" "WorldEnvironment"
                                    "World" "X509Certificate" "XMLParser" "YSort"))

(defun regex-maker (words)
  (regexp-opt words 'symbols))

;;; Font-lock and syntax
(eval-and-compile (defun gdscript-syntax--context-compiler-macro (form type &optional syntax-ppss)
                    (pcase type
                      (''comment
                       `(let ((ppss (or ,syntax-ppss
                                        (syntax-ppss))))
                          (and (nth 4 ppss)
                               (nth 8 ppss))))
                      (''string
                       `(let ((ppss (or ,syntax-ppss
                                        (syntax-ppss))))
                          (and (nth 3 ppss)
                               (nth 8 ppss))))
                      (''paren
                       `(nth 1
                             (or ,syntax-ppss
                                 (syntax-ppss))))
                      (_ form))))

;; Controls font-face mappings or colors to highlight groups of keywords
(defvar gdscript-font-lock `((,(regex-maker gdscript-keywords)
                              1
                              font-lock-keyword-face)
                             (,(regex-maker (concatenate 'list gdscript-built-in-constants
                                                         gdscript-built-in-types gdscript-built-in-functions))
                              1
                              font-lock-builtin-face)
                             (,(regex-maker gdscript-built-in-classes)
                              1
                              font-lock-type-face)
                             (,(rx symbol-start
                                   "func"
                                   (1+ space)
                                   (group (1+ (or word ?_))))
                              (1 font-lock-function-name-face))
                             (,(rx symbol-start
                                   (or "var" "const")
                                   (1+ space)
                                   (group (1+ (or word ?_))))
                              (1 font-lock-variable-name-face))))

(defvar gdscript-syntax-table (make-syntax-table))

(defun gdscript-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro gdscript-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      ('comment (and (nth 4 ppss) (nth 8 ppss)))
      ('string (and (nth 3 ppss) (nth 8 ppss)))
      ('paren (nth 1 ppss))
      (_ nil))))

(defun gdscript-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(define-inline gdscript-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside comment or string."
  (nth 8 (or ppss (syntax-ppss))))

(define-inline gdscript-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (eq (syntax-class (syntax-after (point)))
       (syntax-class (string-to-syntax ")"))))

(defconst gdscript-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (or "\"\"\"" "'''"))
    (0 (ignore (gdscript-syntax-stringify))))))

(define-inline gdscript-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun gdscript-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((ppss (save-excursion (backward-char 3) (syntax-ppss)))
         (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) 3))
         (quote-ending-pos (point)))
    (cond ((or (nth 4 ppss)             ;Inside a comment
               (and string-start
                    ;; Inside of a string quoted with different triple quotes.
                    (not (eq (char-after string-start)
                             (char-after quote-starting-pos)))))
           ;; Do nothing.
           nil)
          ((nth 5 ppss)
           ;; The first quote is escaped, so it's not part of a triple quote!
           (goto-char (1+ quote-starting-pos)))
          ((null string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          (t
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))

(defvar gdscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for Gdscript files.")

(defvar gdscript-dotty-syntax-table
  (let ((table (make-syntax-table gdscript-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Gdscript files.
It makes underscores and dots word constituent chars.")

;;; GDScript regex

(defmacro gdscript-rx (&rest regexps)
  "Gdscript mode specialized rx macro.
This variant of `rx' supports common Gdscript named REGEXPS."
  `(rx-let ((block-start       (seq symbol-start
                                    (or "func" "static" "class" "if" "elif" "else"
                                        "for" "while" "match")
                                    symbol-end))
            (dedenter          (seq symbol-start
                                    (or "elif" "else")
                                    symbol-end))
            (block-ender       (seq symbol-start
                                    (or "break" "continue" "pass" "return")
                                    symbol-end))
            (defun             (seq symbol-start
                                    (or "func" "class" "static func")
                                    symbol-end))
            (symbol-name       (seq (any letter ?_) (* (any word ?_))))
            (open-paren        (or "{" "[" "("))
            (close-paren       (or "}" "]" ")"))
            (simple-operator   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))
            (not-simple-operator (not simple-operator))
            ;; TODO: clean up operators that don't exist in GDScript
            (operator          (or "==" ">=" "is" "not"
                                   "**" "//" "<<" ">>" "<=" "!="
                                   "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                   "=" "%"))
            (assignment-operator (or "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                     ">>=" "<<=" "&=" "^=" "|="
                                     "="))
            (string-delimiter  (seq
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by an
                                    ;; escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"\"\"" "\"" "'''" "'"))))
            )
     (rx ,@regexps)))


;;; Indentation
;; Copied from python.el

;; user customization
(defcustom gdscript-use-tab-indents t
  "Use tabs (t) or spaces (nil)"
  :type 'boolean
  :group 'gdscript)

(defcustom gdscript-tab-width 4
  "Indentation width"
  :type 'integer
  :group 'gdscript)

(defcustom gdscript-indent-offset 4
  "Default indentation offset for Gdscript."
  :group 'gdscript
  :type 'integer
  :safe 'integerp)

(defcustom gdscript-indent-guess-indent-offset t
  "If t, tells GDScript mode to guess `gdscript-indent-offset' value."
  :type 'boolean
  :group 'gdscript
  :safe 'booleanp)

(defcustom gdscript-indent-guess-indent-offset-verbose t
  "If t, emit a warning when guessing indentation fails."
  :version "25.1"
  :type 'boolean
  :group 'gdscript
  :safe' booleanp)

(defcustom gdscript-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `gdscript-indent-line' call."
  :type '(repeat symbol)
  :group 'gdscript)

(defun gdscript-indent-guess-indent-offset ()
  "Guess and set `gdscript-indent-offset' for the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((block-end))
        (while (and (not block-end)
                    (re-search-forward
                     (gdscript-rx line-start block-start) nil t))
          (when (and
                 (not (gdscript-syntax-context-type))
                 (progn
                   (goto-char (line-end-position))
                   (gdscript--util-forward-comment -1)
                   (if (equal (char-before) ?:)
                       t
                     (forward-line 1)
                     (when (gdscript-info-block-continuation-line-p)
                       (while (and (gdscript-info-continuation-line-p)
                                   (not (eobp)))
                         (forward-line 1))
                       (gdscript--util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         t)))))
            (setq block-end (point-marker))))
        (let ((indentation
               (when block-end
                 (goto-char block-end)
                 (gdscript--util-forward-comment)
                 (current-indentation))))
          (if (and indentation (not (zerop indentation)))
              (set (make-local-variable 'gdscript-indent-offset) indentation)
            (when gdscript-indent-guess-indent-offset-verbose
              (message "Can't guess gdscript-indent-offset, using defaults: %s"
                       gdscript-indent-offset))))))))

(defun gdscript-indent-context ()
  "Get information about the current indentation context.
Context is returned in a cons with the form (STATUS . START).

STATUS can be one of the following:

keyword
-------

:after-comment
 - Point is after a comment line.
 - START is the position of the \"#\" character.
:inside-string
 - Point is inside string.
 - START is the position of the first quote that starts it.
:no-indent
 - No possible indentation case matches.
 - START is always zero.

:inside-paren
 - Fallback case when point is inside paren.
 - START is the first non space char position *after* the open paren.
:inside-paren-at-closing-nested-paren
 - Point is on a line that contains a nested paren closer.
 - START is the position of the open paren it closes.
:inside-paren-at-closing-paren
 - Point is on a line that contains a paren closer.
 - START is the position of the open paren.
:inside-paren-newline-start
 - Point is inside a paren with items starting in their own line.
 - START is the position of the open paren.
:inside-paren-newline-start-from-block
 - Point is inside a paren with items starting in their own line
   from a block start.
 - START is the position of the open paren.

:after-backslash
 - Fallback case when point is after backslash.
 - START is the char after the position of the backslash.
:after-backslash-assignment-continuation
 - Point is after a backslashed assignment.
 - START is the char after the position of the backslash.
:after-backslash-block-continuation
 - Point is after a backslashed block continuation.
 - START is the char after the position of the backslash.
:after-backslash-dotted-continuation
 - Point is after a backslashed dotted continuation.  Previous
   line must contain a dot to align with.
 - START is the char after the position of the backslash.
:after-backslash-first-line
 - First line following a backslashed continuation.
 - START is the char after the position of the backslash.

:after-block-end
 - Point is after a line containing a block ender.
 - START is the position where the ender starts.
:after-block-start
 - Point is after a line starting a block.
 - START is the position where the block starts.
:after-line
 - Point is after a simple line.
 - START is the position where the previous line starts.
:at-dedenter-block-start
 - Point is on a line starting a dedenter block.
 - START is the position where the dedenter block starts."
    (let ((ppss (save-excursion
                  (beginning-of-line)
                  (syntax-ppss))))
      (cond
       ;; Beginning of buffer.
       ((= (line-number-at-pos) 1)
        (cons :no-indent 0))
       ;; Inside a string.
       ((let ((start (gdscript-syntax-context 'string ppss)))
          (when start
            (cons :inside-string start))))
       ;; Inside a paren.
       ((let* ((start (gdscript-syntax-context 'paren ppss))
               (starts-in-newline
                (when start
                  (save-excursion
                    (goto-char start)
                    (forward-char)
                    (not
                     (= (line-number-at-pos)
                        (progn
                          (gdscript--util-forward-comment)
                          (line-number-at-pos))))))))
          (when start
            (cond
             ;; Current line only holds the closing paren.
             ((save-excursion
                (skip-syntax-forward " ")
                (when (and (gdscript-syntax-closing-paren-p)
                           (progn
                             (forward-char 1)
                             (not (gdscript-syntax-context 'paren))))
                  (cons :inside-paren-at-closing-paren start))))
             ;; Current line only holds a closing paren for nested.
             ((save-excursion
                (back-to-indentation)
                (gdscript-syntax-closing-paren-p))
              (cons :inside-paren-at-closing-nested-paren start))
             ;; This line starts from an opening block in its own line.
             ((save-excursion
                (goto-char start)
                (when (and
                       starts-in-newline
                       (save-excursion
                         (back-to-indentation)
                         (looking-at (gdscript-rx block-start))))
                  (cons
                   :inside-paren-newline-start-from-block start))))
             (starts-in-newline
              (cons :inside-paren-newline-start start))
             ;; General case.
             (t (cons :inside-paren
                      (save-excursion
                        (goto-char (1+ start))
                        (skip-syntax-forward "(" 1)
                        (skip-syntax-forward " ")
                        (point))))))))
       ;; After backslash.
       ((let ((start (when (not (gdscript-syntax-comment-or-string-p ppss))
                       (gdscript-info-line-ends-backslash-p
                        (1- (line-number-at-pos))))))
          (when start
            (cond
             ;; Continuation of dotted expression.
             ((save-excursion
                (back-to-indentation)
                (when (eq (char-after) ?\.)
                  ;; Move point back until it's not inside a paren.
                  (while (prog2
                             (forward-line -1)
                             (and (not (bobp))
                                  (gdscript-syntax-context 'paren))))
                  (goto-char (line-end-position))
                  (while (and (search-backward
                               "." (line-beginning-position) t)
                              (gdscript-syntax-context-type)))
                  ;; Ensure previous statement has dot to align with.
                  (when (and (eq (char-after) ?\.)
                             (not (gdscript-syntax-context-type)))
                    (cons :after-backslash-dotted-continuation (point))))))
             ;; Continuation of block definition.
             ((let ((block-continuation-start
                     (gdscript-info-block-continuation-line-p)))
                (when block-continuation-start
                  (save-excursion
                    (goto-char block-continuation-start)
                    (re-search-forward
                     (gdscript-rx block-start (* space))
                     (line-end-position) t)
                    (cons :after-backslash-block-continuation (point))))))
             ;; Continuation of assignment.
             ((let ((assignment-continuation-start
                     (gdscript-info-assignment-continuation-line-p)))
                (when assignment-continuation-start
                  (save-excursion
                    (goto-char assignment-continuation-start)
                    (cons :after-backslash-assignment-continuation (point))))))
             ;; First line after backslash continuation start.
             ((save-excursion
                (goto-char start)
                (when (or (= (line-number-at-pos) 1)
                          (not (gdscript-info-beginning-of-backslash
                                (1- (line-number-at-pos)))))
                  (cons :after-backslash-first-line start))))
             ;; General case.
             (t (cons :after-backslash start))))))
       ;; After beginning of block.
       ((let ((start (save-excursion
                       (back-to-indentation)
                       (gdscript--util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         (gdscript-nav-beginning-of-block)))))
          (when start
            (cons :after-block-start start))))
       ;; At dedenter statement.
       ((let ((start (gdscript-info-dedenter-statement-p)))
          (when start
            (cons :at-dedenter-block-start start))))
       ;; After normal line, comment or ender (default case).
       ((save-excursion
          (back-to-indentation)
          (skip-chars-backward " \t\n")
          (if (bobp)
              (cons :no-indent 0)
            (gdscript-nav-beginning-of-statement)
            (cons
             (cond ((gdscript-info-current-line-comment-p)
                    :after-comment)
                   ((save-excursion
                      (goto-char (line-end-position))
                      (gdscript--util-forward-comment -1)
                      (gdscript-nav-beginning-of-statement)
                      (looking-at (gdscript-rx block-ender)))
                    :after-block-end)
                   (t :after-line))
             (point))))))))

(defun gdscript-indent--calculate-indentation ()
  "Internal implementation of `gdscript-indent-calculate-indentation'.
May return an integer for the maximum possible indentation at
current context or a list of integers.  The latter case is only
happening for :at-dedenter-block-start context since the
possibilities can be narrowed to specific indentation points."
    (save-excursion
      (pcase (gdscript-indent-context)
        (`(:no-indent . ,_) (prog-first-column)) ; usually 0
        (`(,(or :after-line
                :after-comment
                :inside-string
                :after-backslash) . ,start)
         ;; Copy previous indentation.
         (goto-char start)
         (current-indentation))
        (`(,(or :inside-paren-at-closing-paren
                :inside-paren-at-closing-nested-paren) . ,start)
         (goto-char (+ 1 start))
         (if (looking-at "[ \t]*\\(?:#\\|$\\)")
             ;; Copy previous indentation.
             (current-indentation)
           ;; Align with opening paren.
           (current-column)))
        (`(,(or :after-block-start
                :after-backslash-first-line
                :after-backslash-assignment-continuation
                :inside-paren-newline-start) . ,start)
         ;; Add one indentation level.
         (goto-char start)
         (+ (current-indentation) gdscript-indent-offset))
        (`(,(or :inside-paren
                :after-backslash-block-continuation
                :after-backslash-dotted-continuation) . ,start)
         ;; Use the column given by the context.
         (goto-char start)
         (current-column))
        (`(:after-block-end . ,start)
         ;; Subtract one indentation level.
         (goto-char start)
         (- (current-indentation) gdscript-indent-offset))
        (`(:at-dedenter-block-start . ,_)
         ;; List all possible indentation levels from opening blocks.
         (let ((opening-block-start-points
                (gdscript-info-dedenter-opening-block-positions)))
           (if (not opening-block-start-points)
               (prog-first-column) ; if not found default to first column
             (mapcar (lambda (pos)
                       (save-excursion
                         (goto-char pos)
                         (current-indentation)))
                     opening-block-start-points))))
        (`(,(or :inside-paren-newline-start-from-block) . ,start)
         (goto-char start)
         (+ (current-indentation)
            (* gdscript-indent-offset gdscript-indent-def-block-scale))))))

(defun gdscript-indent--calculate-levels (indentation)
  "Calculate levels list given INDENTATION.
Argument INDENTATION can either be an integer or a list of
integers.  Levels are returned in ascending order, and in the
case INDENTATION is a list, this order is enforced."
  (if (listp indentation)
      (sort (copy-sequence indentation) #'<)
    (nconc (number-sequence (prog-first-column) (1- indentation)
                            gdscript-indent-offset)
           (list indentation))))

(defun gdscript-indent--previous-level (levels indentation)
  "Return previous level from LEVELS relative to INDENTATION."
  (let* ((levels (sort (copy-sequence levels) #'>))
         (default (car levels)))
    (catch 'return
      (dolist (level levels)
        (when (funcall #'< level indentation)
          (throw 'return level)))
      default)))

(defun gdscript-indent-calculate-indentation (&optional previous)
  "Calculate indentation.
Get indentation of PREVIOUS level when argument is non-nil.
Return the max level of the cycle when indentation reaches the
minimum."
  (let* ((indentation (gdscript-indent--calculate-indentation))
         (levels (gdscript-indent--calculate-levels indentation)))
    (if previous
        (gdscript-indent--previous-level levels (current-indentation))
      (if levels
          (apply #'max levels)
        (prog-first-column)))))

(defun gdscript-indent-line (&optional previous)
  "Internal implementation of `gdscript-indent-line-function'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level."
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to
       (gdscript-indent-calculate-indentation previous))
      (gdscript-info-dedenter-opening-block-message))
    (when follow-indentation-p
      (back-to-indentation))))

(defun gdscript-indent-calculate-levels ()
  "Return possible indentation levels."
  (gdscript-indent--calculate-levels
   (gdscript-indent--calculate-indentation)))

(defun gdscript-indent-line-function ()
  "`indent-line-function' for Gdscript mode.
When the variable `last-command' is equal to one of the symbols
inside `gdscript-indent-trigger-commands' it cycles possible
indentation levels from right to left."
  (gdscript-indent-line
   (and (memq this-command gdscript-indent-trigger-commands)
        (eq last-command this-command))))

(defun gdscript-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp))
           (not (gdscript-syntax-comment-or-string-p))
           (= (current-indentation) (current-column)))
      (gdscript-indent-line t)
      t))

(defun gdscript-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is not in between the indentation."
  (interactive "*p")
  (unless (gdscript-indent-dedent-line)
    (backward-delete-char-untabify arg)))

(put 'gdscript-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun gdscript-indent-region (start end)
  "Indent a Gdscript region automagically.

Called from a program, START and END specify the region to indent."
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (when (and
                   ;; Skip if previous line is empty or a comment.
                   (save-excursion
                     (let ((line-is-comment-p
                            (gdscript-info-current-line-comment-p)))
                       (forward-line -1)
                       (not
                        (or (and (gdscript-info-current-line-comment-p)
                                 ;; Unless this line is a comment too.
                                 (not line-is-comment-p))
                            (gdscript-info-current-line-empty-p)))))
                   ;; Skip if current line is a block start, a
                   ;; dedenter or block ender.
                   (save-excursion
                     (back-to-indentation)
                     (not (looking-at
                           (gdscript-rx
                            (or block-start dedenter block-ender))))))
              (gdscript-indent-line)))
        (forward-line 1))
      (move-marker end nil))))

(defun gdscript-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `gdscript-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count gdscript-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (user-error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(defun gdscript-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
COUNT defaults to `gdscript-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count)
                  gdscript-indent-offset))
    (indent-rigidly start end count)))

(defun gdscript-indent-post-self-insert-function ()
  "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event)
             (not (gdscript-syntax-context 'string))
             (save-excursion
               (beginning-of-line)
               (not (gdscript-syntax-context 'string (syntax-ppss)))))
    (cond
     ;; Electric indent inside parens
     ((and
       (not (bolp))
       (let ((paren-start (gdscript-syntax-context 'paren)))
         ;; Check that point is inside parens.
         (when paren-start
           (not
            ;; Filter the case where input is happening in the same
            ;; line where the open paren is.
            (= (line-number-at-pos)
               (line-number-at-pos paren-start)))))
       ;; When content has been added before the closing paren or a
       ;; comma has been inserted, it's ok to do the trick.
       (or
        (memq (char-after) '(?\) ?\] ?\}))
        (eq (char-before) ?,)))
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((indentation (gdscript-indent-calculate-indentation)))
          (when (and (numberp indentation) (< (current-indentation) indentation))
            (indent-line-to indentation)))))
     ;; Electric colon
     ((and (eq ?: last-command-event)
           (memq ?: electric-indent-chars)
           (not current-prefix-arg)
           ;; Trigger electric colon only at end of line
           (eolp)
           ;; Avoid re-indenting on extra colon
           (not (equal ?: (char-before (1- (point)))))
           (not (gdscript-syntax-comment-or-string-p)))
      ;; Just re-indent dedenters
      (let ((dedenter-pos (gdscript-info-dedenter-statement-p)))
        (when dedenter-pos
          (let ((start (copy-marker dedenter-pos))
                (end (point-marker)))
            (save-excursion
              (goto-char start)
              (gdscript-indent-line)
              (unless (= (line-number-at-pos start)
                         (line-number-at-pos end))
                ;; Reindent region if this is a multiline statement
                (gdscript-indent-region start end))))))))))


;;; Misc helpers

(defun gdscript-info-current-defun (&optional include-type)
  "Return name of surrounding function with Gdscript compatible dotty syntax.
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function can be used as the value of `add-log-current-defun-function'
since it returns nil if point is not inside a defun."
  (save-restriction
    (widen)
    (save-excursion
      (end-of-line 1)
      (let ((names)
            (starting-indentation (current-indentation))
            (starting-pos (point))
            (first-run t)
            (last-indent)
            (type))
        (catch 'exit
          (while (gdscript-nav-beginning-of-defun 1)
            (when (save-match-data
                    (and
                     (or (not last-indent)
                         (< (current-indentation) last-indent))
                     (or
                      (and first-run
                           (save-excursion
                             ;; If this is the first run, we may add
                             ;; the current defun at point.
                             (setq first-run nil)
                             (goto-char starting-pos)
                             (gdscript-nav-beginning-of-statement)
                             (beginning-of-line 1)
                             (looking-at-p
                              gdscript-nav-beginning-of-defun-regexp)))
                      (< starting-pos
                         (save-excursion
                           (let ((min-indent
                                  (+ (current-indentation)
                                     gdscript-indent-offset)))
                             (if (< starting-indentation  min-indent)
                                 ;; If the starting indentation is not
                                 ;; within the min defun indent make the
                                 ;; check fail.
                                 starting-pos
                               ;; Else go to the end of defun and add
                               ;; up the current indentation to the
                               ;; ending position.
                               (gdscript-nav-end-of-defun)
                               (+ (point)
                                  (if (>= (current-indentation) min-indent)
                                      (1+ (current-indentation))
                                    0)))))))))
              (save-match-data (setq last-indent (current-indentation)))
              (if (or (not include-type) type)
                  (setq names (cons (match-string-no-properties 1) names))
                (let ((match (split-string (match-string-no-properties 0))))
                  (setq type (car match))
                  (setq names (cons (cadr match) names)))))
            ;; Stop searching ASAP.
            (and (= (current-indentation) 0) (throw 'exit t))))
        (and names
             (concat (and type (format "%s " type))
                     (mapconcat 'identity names ".")))))))

(defun gdscript-info-current-symbol (&optional replace-self)
  "Return current symbol using dotty syntax.
With optional argument REPLACE-SELF convert \"self\" to current
parent defun name."
  (let ((name
         (and (not (gdscript-syntax-comment-or-string-p))
              (with-syntax-table gdscript-dotty-syntax-table
                (let ((sym (symbol-at-point)))
                  (and sym
                       (substring-no-properties (symbol-name sym))))))))
    (when name
      (if (not replace-self)
          name
        (let ((current-defun (gdscript-info-current-defun)))
          (if (not current-defun)
              name
            (replace-regexp-in-string
             (gdscript-rx line-start word-start "self" word-end ?.)
             (concat
              (mapconcat 'identity
                         (butlast (split-string current-defun "\\."))
                         ".") ".")
             name)))))))

(defun gdscript-info-statement-starts-block-p ()
  "Return non-nil if current statement opens a block."
  (save-excursion
    (gdscript-nav-beginning-of-statement)
    (looking-at (gdscript-rx block-start))))

(defun gdscript-info-statement-ends-block-p ()
  "Return non-nil if point is at end of block."
  (let ((end-of-block-pos (save-excursion
                            (gdscript-nav-end-of-block)))
        (end-of-statement-pos (save-excursion
                                (gdscript-nav-end-of-statement))))
    (and end-of-block-pos end-of-statement-pos
         (= end-of-block-pos end-of-statement-pos))))

(defun gdscript-info-beginning-of-statement-p ()
  "Return non-nil if point is at beginning of statement."
  (= (point) (save-excursion
               (gdscript-nav-beginning-of-statement)
               (point))))

(defun gdscript-info-end-of-statement-p ()
  "Return non-nil if point is at end of statement."
  (= (point) (save-excursion
               (gdscript-nav-end-of-statement)
               (point))))

(defun gdscript-info-beginning-of-block-p ()
  "Return non-nil if point is at beginning of block."
  (and (gdscript-info-beginning-of-statement-p)
       (gdscript-info-statement-starts-block-p)))

(defun gdscript-info-end-of-block-p ()
  "Return non-nil if point is at end of block."
  (and (gdscript-info-end-of-statement-p)
       (gdscript-info-statement-ends-block-p)))

(defun gdscript-info-dedenter-opening-block-position ()
  "Return the point of the closest block the current line closes.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid gdscript file."
  (let ((positions (gdscript-info-dedenter-opening-block-positions))
        (indentation (current-indentation))
        (position))
    (while (and (not position)
                positions)
      (save-excursion
        (goto-char (car positions))
        (if (<= (current-indentation) indentation)
            (setq position (car positions))
          (setq positions (cdr positions)))))
    position))

(defun gdscript-info-dedenter-opening-block-positions ()
  "Return points of blocks the current line may close sorted by closer.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid gdscript file."
  (save-excursion
    (let ((dedenter-pos (gdscript-info-dedenter-statement-p)))
      (when dedenter-pos
        (goto-char dedenter-pos)
        (let* ((cur-line (line-beginning-position))
               (pairs '(("elif" "elif" "if")
                        ("else" "if" "elif" "except" "for" "while")
                        ("except" "except" "try")
                        ("finally" "else" "except" "try")))
               (dedenter (match-string-no-properties 0))
               (possible-opening-blocks (cdr (assoc-string dedenter pairs)))
               (collected-indentations)
               (opening-blocks))
          (catch 'exit
            (while (gdscript-nav--syntactically
                    (lambda ()
                      (re-search-backward (gdscript-rx block-start) nil t))
                    #'<)
              (let ((indentation (current-indentation)))
                (when (and (not (memq indentation collected-indentations))
                           (or (not collected-indentations)
                               (< indentation (apply #'min collected-indentations)))
                           ;; There must be no line with indentation
                           ;; smaller than `indentation' (except for
                           ;; blank lines) between the found opening
                           ;; block and the current line, otherwise it
                           ;; is not an opening block.
                           (save-excursion
                             (forward-line)
                             (let ((no-back-indent t))
                               (save-match-data
                                 (while (and (< (point) cur-line)
                                             (setq no-back-indent
                                                   (or (> (current-indentation) indentation)
                                                       (gdscript-info-current-line-empty-p))))
                                   (forward-line)))
                               no-back-indent)))
                  (setq collected-indentations
                        (cons indentation collected-indentations))
                  (when (member (match-string-no-properties 0)
                                possible-opening-blocks)
                    (setq opening-blocks (cons (point) opening-blocks))))
                (when (zerop indentation)
                  (throw 'exit nil)))))
          ;; sort by closer
          (nreverse opening-blocks))))))

(defun gdscript-info-dedenter-opening-block-message  ()
  "Message the first line of the block the current statement closes."
  (let ((point (gdscript-info-dedenter-opening-block-position)))
    (when point
        (message "Closes %s" (save-excursion
                               (goto-char point)
                               (buffer-substring
                                (point) (line-end-position)))))))

(defun gdscript-info-dedenter-statement-p ()
  "Return point if current statement is a dedenter.
Sets `match-data' to the keyword that starts the dedenter
statement."
  (save-excursion
    (gdscript-nav-beginning-of-statement)
    (when (and (not (gdscript-syntax-context-type))
               (looking-at (gdscript-rx dedenter)))
      (point))))

(defun gdscript-info-line-ends-backslash-p (&optional line-number)
  "Return non-nil if current line ends with backslash.
With optional argument LINE-NUMBER, check that line instead."
  (save-excursion
      (when line-number
        (gdscript--util-goto-line line-number))
      (while (and (not (eobp))
                  (goto-char (line-end-position))
                  (gdscript-syntax-context 'paren)
                  (not (equal (char-before (point)) ?\\)))
        (forward-line 1))
      (when (equal (char-before) ?\\)
        (point-marker))))

(defun gdscript-info-beginning-of-backslash (&optional line-number)
  "Return the point where the backslashed line starts.
Optional argument LINE-NUMBER forces the line number to check against."
  (save-excursion
      (when line-number
        (gdscript--util-goto-line line-number))
      (when (gdscript-info-line-ends-backslash-p)
        (while (save-excursion
                 (goto-char (line-beginning-position))
                 (gdscript-syntax-context 'paren))
          (forward-line -1))
        (back-to-indentation)
        (point-marker))))

(defun gdscript-info-continuation-line-p ()
  "Check if current line is continuation of another.
When current line is continuation of another return the point
where the continued line ends."
  (save-excursion
      (let* ((context-type (progn
                             (back-to-indentation)
                             (gdscript-syntax-context-type)))
             (line-start (line-number-at-pos))
             (context-start (when context-type
                              (gdscript-syntax-context context-type))))
        (cond ((equal context-type 'paren)
               ;; Lines inside a paren are always a continuation line
               ;; (except the first one).
               (gdscript--util-forward-comment -1)
               (point-marker))
              ((member context-type '(string comment))
               ;; move forward an roll again
               (goto-char context-start)
               (gdscript--util-forward-comment)
               (gdscript-info-continuation-line-p))
              (t
               ;; Not within a paren, string or comment, the only way
               ;; we are dealing with a continuation line is that
               ;; previous line contains a backslash, and this can
               ;; only be the previous line from current
               (back-to-indentation)
               (gdscript--util-forward-comment -1)
               (when (and (equal (1- line-start) (line-number-at-pos))
                          (gdscript-info-line-ends-backslash-p))
                 (point-marker)))))))

(defun gdscript-info-block-continuation-line-p ()
  "Return non-nil if current line is a continuation of a block."
  (save-excursion
    (when (gdscript-info-continuation-line-p)
      (forward-line -1)
      (back-to-indentation)
      (when (looking-at (gdscript-rx block-start))
        (point-marker)))))

(defun gdscript-info-assignment-statement-p (&optional current-line-only)
  "Check if current line is an assignment.
With argument CURRENT-LINE-ONLY is non-nil, don't follow any
continuations, just check the if current line is an assignment."
  (save-excursion
    (let ((found nil))
      (if current-line-only
          (back-to-indentation)
        (gdscript-nav-beginning-of-statement))
      (while (and
              (re-search-forward (gdscript-rx not-simple-operator
                                            assignment-operator
                                            (group not-simple-operator))
                                 (line-end-position) t)
              (not found))
        (save-excursion
          ;; The assignment operator should not be inside a string.
          (backward-char (length (match-string-no-properties 1)))
          (setq found (not (gdscript-syntax-context-type)))))
      (when found
        (skip-syntax-forward " ")
        (point-marker)))))

;; TODO: rename to clarify this is only for the first continuation
;; line or remove it and move its body to `gdscript-indent-context'.
(defun gdscript-info-assignment-continuation-line-p ()
  "Check if current line is the first continuation of an assignment.
When current line is continuation of another with an assignment
return the point of the first non-blank character after the
operator."
  (save-excursion
    (when (gdscript-info-continuation-line-p)
      (forward-line -1)
      (gdscript-info-assignment-statement-p t))))

(defun gdscript-info-looking-at-beginning-of-defun (&optional syntax-ppss)
  "Check if point is at `beginning-of-defun' using SYNTAX-PPSS."
  (and (not (gdscript-syntax-context-type (or syntax-ppss (syntax-ppss))))
       (save-excursion
         (beginning-of-line 1)
         (looking-at gdscript-nav-beginning-of-defun-regexp))))

(defun gdscript-info-current-line-comment-p ()
  "Return non-nil if current line is a comment line."
  (char-equal
   (or (char-after (+ (line-beginning-position) (current-indentation))) ?_)
   ?#))

(defun gdscript-info-current-line-empty-p ()
  "Return non-nil if current line is empty, ignoring whitespace."
  (save-excursion
    (beginning-of-line 1)
    (looking-at
     (gdscript-rx line-start (* whitespace)
                (group (* not-newline))
                (* whitespace) line-end))
    (string-equal "" (match-string-no-properties 1))))


;;; Utility functions

(defun gdscript--util-goto-line (line-number)
  "Move point to LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun gdscript--util-forward-comment (&optional direction)
  "Gdscript mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (gdscript-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

(defun gdscript--util-list-directories (directory &optional predicate max-depth)
  "List DIRECTORY subdirs, filtered by PREDICATE and limited by MAX-DEPTH.
Argument PREDICATE defaults to `identity' and must be a function
that takes one argument (a full path) and returns non-nil for
allowed files.  When optional argument MAX-DEPTH is non-nil, stop
searching when depth is reached, else don't limit."
  (let* ((dir (expand-file-name directory))
         (dir-length (length dir))
         (predicate (or predicate #'identity))
         (to-scan (list dir))
         (tally nil))
    (while to-scan
      (let ((current-dir (car to-scan)))
        (when (funcall predicate current-dir)
          (setq tally (cons current-dir tally)))
        (setq to-scan (append (cdr to-scan)
                              (gdscript--util-list-files
                               current-dir #'file-directory-p)
                              nil))
        (when (and max-depth
                   (<= max-depth
                       (length (split-string
                                (substring current-dir dir-length)
                                "/\\|\\\\" t))))
          (setq to-scan nil))))
    (nreverse tally)))

(defun gdscript--util-list-files (dir &optional predicate)
  "List files in DIR, filtering with PREDICATE.
Argument PREDICATE defaults to `identity' and must be a function
that takes one argument (a full path) and returns non-nil for
allowed files."
  (let ((dir-name (file-name-as-directory dir)))
    (apply #'nconc
           (mapcar (lambda (file-name)
                     (let ((full-file-name (expand-file-name file-name dir-name)))
                       (when (and
                              (not (member file-name '("." "..")))
                              (funcall (or predicate #'identity) full-file-name))
                         (list full-file-name))))
                   (directory-files dir-name)))))


;;; Navigation

(defvar gdscript-nav-beginning-of-defun-regexp
  (gdscript-rx line-start (* space) defun (+ space) (group symbol-name))
  "Regexp matching class or function definition.
The name of the defun should be grouped so it can be retrieved
via `match-string'.")

(defun gdscript-nav--beginning-of-defun (&optional arg)
  "Internal implementation of `gdscript-nav-beginning-of-defun'.
With positive ARG search backwards, else search forwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let* ((re-search-fn (if (> arg 0)
                           #'re-search-backward
                         #'re-search-forward))
         (line-beg-pos (line-beginning-position))
         (line-content-start (+ line-beg-pos (current-indentation)))
         (pos (point-marker))
         (beg-indentation
          (and (> arg 0)
               (save-excursion
                 (while (and
                         (not (gdscript-info-looking-at-beginning-of-defun))
                         (gdscript-nav-backward-block)))
                 (or (and (gdscript-info-looking-at-beginning-of-defun)
                          (+ (current-indentation) gdscript-indent-offset))
                     0))))
         (found
          (progn
            (when (and (< arg 0)
                       (gdscript-info-looking-at-beginning-of-defun))
              (end-of-line 1))
            (while (and (funcall re-search-fn
                                 gdscript-nav-beginning-of-defun-regexp nil t)
                        (or (gdscript-syntax-context-type)
                            ;; Handle nested defuns when moving
                            ;; backwards by checking indentation.
                            (and (> arg 0)
                                 (not (= (current-indentation) 0))
                                 (>= (current-indentation) beg-indentation)))))
            (and (gdscript-info-looking-at-beginning-of-defun)
                 (or (not (= (line-number-at-pos pos)
                             (line-number-at-pos)))
                     (and (>= (point) line-beg-pos)
                          (<= (point) line-content-start)
                          (> pos line-content-start)))))))
    (if found
        (or (beginning-of-line 1) t)
      (and (goto-char pos) nil))))

(defun gdscript-nav-beginning-of-defun (&optional arg)
  "Move point to `beginning-of-defun'.
With positive ARG search backwards else search forward.
ARG nil or 0 defaults to 1.  When searching backwards,
nested defuns are handled with care depending on current
point position.  Return non-nil if point is moved to
`beginning-of-defun'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((found))
    (while (and (not (= arg 0))
                (let ((keep-searching-p
                       (gdscript-nav--beginning-of-defun arg)))
                  (when (and keep-searching-p (null found))
                    (setq found t))
                  keep-searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun gdscript-nav-end-of-defun ()
  "Move point to the end of def or class.
Returns nil if point is not in a def or class."
  (interactive)
  (let ((beg-defun-indent)
        (beg-pos (point)))
    (when (or (gdscript-info-looking-at-beginning-of-defun)
              (gdscript-nav-beginning-of-defun 1)
              (gdscript-nav-beginning-of-defun -1))
      (setq beg-defun-indent (current-indentation))
      (while (progn
               (gdscript-nav-end-of-statement)
               (gdscript--util-forward-comment 1)
               (and (> (current-indentation) beg-defun-indent)
                    (not (eobp)))))
      (gdscript--util-forward-comment -1)
      (forward-line 1)
      ;; Ensure point moves forward.
      (and (> beg-pos (point)) (goto-char beg-pos)))))

(defun gdscript-nav--syntactically (fn poscompfn &optional contextfn)
  "Move point using FN avoiding places with specific context.
FN must take no arguments.  POSCOMPFN is a two arguments function
used to compare current and previous point after it is moved
using FN, this is normally a less-than or greater-than
comparison.  Optional argument CONTEXTFN defaults to
`gdscript-syntax-context-type' and is used for checking current
point context, it must return a non-nil value if this point must
be skipped."
  (let ((contextfn (or contextfn 'gdscript-syntax-context-type))
        (start-pos (point-marker))
        (prev-pos))
    (catch 'found
      (while t
        (let* ((newpos
                (and (funcall fn) (point-marker)))
               (context (funcall contextfn)))
          (cond ((and (not context) newpos
                      (or (and (not prev-pos) newpos)
                          (and prev-pos newpos
                               (funcall poscompfn newpos prev-pos))))
                 (throw 'found (point-marker)))
                ((and newpos context)
                 (setq prev-pos (point)))
                (t (when (not newpos) (goto-char start-pos))
                   (throw 'found nil))))))))

(defun gdscript-nav--forward-defun (arg)
  "Internal implementation of gdscript-nav-{backward,forward}-defun.
Uses ARG to define which function to call, and how many times
repeat it."
  (let ((found))
    (while (and (> arg 0)
                (setq found
                      (gdscript-nav--syntactically
                       (lambda ()
                         (re-search-forward
                          gdscript-nav-beginning-of-defun-regexp nil t))
                       '>)))
      (setq arg (1- arg)))
    (while (and (< arg 0)
                (setq found
                      (gdscript-nav--syntactically
                       (lambda ()
                         (re-search-backward
                          gdscript-nav-beginning-of-defun-regexp nil t))
                       '<)))
      (setq arg (1+ arg)))
    found))

(defun gdscript-nav-backward-defun (&optional arg)
  "Navigate to closer defun backward ARG times.
Unlikely `gdscript-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (gdscript-nav--forward-defun (- (or arg 1))))

(defun gdscript-nav-forward-defun (&optional arg)
  "Navigate to closer defun forward ARG times.
Unlikely `gdscript-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (gdscript-nav--forward-defun (or arg 1)))

(defun gdscript-nav-beginning-of-statement ()
  "Move to start of current statement."
  (interactive "^")
  (forward-line 0)
  (let* ((ppss (syntax-ppss))
         (context-point
          (or
           (gdscript-syntax-context 'paren ppss)
           (gdscript-syntax-context 'string ppss))))
    (cond ((bobp))
          (context-point
           (goto-char context-point)
           (gdscript-nav-beginning-of-statement))
          ((save-excursion
             (forward-line -1)
             (gdscript-info-line-ends-backslash-p))
           (forward-line -1)
           (gdscript-nav-beginning-of-statement))))
  (back-to-indentation)
  (point-marker))

(defun gdscript-nav-end-of-statement (&optional noend)
  "Move to end of current statement.
Optional argument NOEND is internal and makes the logic to not
jump to the end of line when moving forward searching for the end
of the statement."
  (interactive "^")
  (let (string-start bs-pos (last-string-end 0))
    (while (and (or noend (goto-char (line-end-position)))
                (not (eobp))
                (cond ((setq string-start (gdscript-syntax-context 'string))
                       ;; The assertion can only fail if syntax table
                       ;; text properties and the `syntax-ppss' cache
                       ;; are somehow out of whack.  This has been
                       ;; observed when using `syntax-ppss' during
                       ;; narrowing.
                       (cl-assert (>= string-start last-string-end)
                                  :show-args
                                  "\
Overlapping strings detected (start=%d, last-end=%d)")
                       (goto-char string-start)
                       (if (gdscript-syntax-context 'paren)
                           ;; Ended up inside a paren, roll again.
                           (gdscript-nav-end-of-statement t)
                         ;; This is not inside a paren, move to the
                         ;; end of this string.
                         (goto-char (+ (point)
                                       (gdscript-syntax-count-quotes
                                        (char-after (point)) (point))))
                         (setq last-string-end
                               (or (re-search-forward
                                    (rx (syntax string-delimiter)) nil t)
                                   (goto-char (point-max))))))
                      ((gdscript-syntax-context 'paren)
                       ;; The statement won't end before we've escaped
                       ;; at least one level of parenthesis.
                       (condition-case err
                           (goto-char (scan-lists (point) 1 -1))
                         (scan-error (goto-char (nth 3 err)))))
                      ((setq bs-pos (gdscript-info-line-ends-backslash-p))
                       (goto-char bs-pos)
                       (forward-line 1))))))
  (point-marker))

(defun gdscript-nav-backward-statement (&optional arg)
  "Move backward to previous statement.
With ARG, repeat.  See `gdscript-nav-forward-statement'."
  (interactive "^p")
  (or arg (setq arg 1))
  (gdscript-nav-forward-statement (- arg)))

(defun gdscript-nav-forward-statement (&optional arg)
  "Move forward to next statement.
With ARG, repeat.  With negative argument, move ARG times
backward to previous statement."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (gdscript-nav-end-of-statement)
    (gdscript--util-forward-comment)
    (gdscript-nav-beginning-of-statement)
    (setq arg (1- arg)))
  (while (< arg 0)
    (gdscript-nav-beginning-of-statement)
    (gdscript--util-forward-comment -1)
    (gdscript-nav-beginning-of-statement)
    (setq arg (1+ arg))))

(defun gdscript-nav-beginning-of-block ()
  "Move to start of current block."
  (interactive "^")
  (let ((starting-pos (point)))
    (if (progn
          (gdscript-nav-beginning-of-statement)
          (looking-at (gdscript-rx block-start)))
        (point-marker)
      ;; Go to first line beginning a statement
      (while (and (not (bobp))
                  (or (and (gdscript-nav-beginning-of-statement) nil)
                      (gdscript-info-current-line-comment-p)
                      (gdscript-info-current-line-empty-p)))
        (forward-line -1))
      (let ((block-matching-indent
             (- (current-indentation) gdscript-indent-offset)))
        (while
            (and (gdscript-nav-backward-block)
                 (> (current-indentation) block-matching-indent)))
        (if (and (looking-at (gdscript-rx block-start))
                 (= (current-indentation) block-matching-indent))
            (point-marker)
          (and (goto-char starting-pos) nil))))))

(defun gdscript-nav-end-of-block ()
  "Move to end of current block."
  (interactive "^")
  (when (gdscript-nav-beginning-of-block)
    (let ((block-indentation (current-indentation)))
      (gdscript-nav-end-of-statement)
      (while (and (forward-line 1)
                  (not (eobp))
                  (or (and (> (current-indentation) block-indentation)
                           (or (gdscript-nav-end-of-statement) t))
                      (gdscript-info-current-line-comment-p)
                      (gdscript-info-current-line-empty-p))))
      (gdscript--util-forward-comment -1)
      (point-marker))))

(defun gdscript-nav-backward-block (&optional arg)
  "Move backward to previous block of code.
With ARG, repeat.  See `gdscript-nav-forward-block'."
  (interactive "^p")
  (or arg (setq arg 1))
  (gdscript-nav-forward-block (- arg)))

(defun gdscript-nav-forward-block (&optional arg)
  "Move forward to next block of code.
With ARG, repeat.  With negative argument, move ARG times
backward to previous block."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((block-start-regexp
         (gdscript-rx line-start (* whitespace) block-start))
        (starting-pos (point)))
    (while (> arg 0)
      (gdscript-nav-end-of-statement)
      (while (and
              (re-search-forward block-start-regexp nil t)
              (gdscript-syntax-context-type)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (gdscript-nav-beginning-of-statement)
      (while (and
              (re-search-backward block-start-regexp nil t)
              (gdscript-syntax-context-type)))
      (setq arg (1+ arg)))
    (gdscript-nav-beginning-of-statement)
    (if (not (looking-at (gdscript-rx block-start)))
        (and (goto-char starting-pos) nil)
      (and (not (= (point) starting-pos)) (point-marker)))))

(defun gdscript-nav--lisp-forward-sexp (&optional arg)
  "Standard version `forward-sexp'.
It ignores completely the value of `forward-sexp-function' by
setting it to nil before calling `forward-sexp'.  With positive
ARG move forward only one sexp, else move backwards."
  (let ((forward-sexp-function)
        (arg (if (or (not arg) (> arg 0)) 1 -1)))
    (forward-sexp arg)))

(defun gdscript-nav--lisp-forward-sexp-safe (&optional arg)
  "Safe version of standard `forward-sexp'.
When at end of sexp (i.e. looking at an opening/closing paren)
skips it instead of throwing an error.  With positive ARG move
forward only one sexp, else move backwards."
  (let* ((arg (if (or (not arg) (> arg 0)) 1 -1))
         (paren-regexp
          (if (> arg 0) (gdscript-rx close-paren) (gdscript-rx open-paren)))
         (search-fn
          (if (> arg 0) #'re-search-forward #'re-search-backward)))
    (condition-case nil
        (gdscript-nav--lisp-forward-sexp arg)
      (error
       (while (and (funcall search-fn paren-regexp nil t)
                   (gdscript-syntax-context 'paren)))))))

(defun gdscript-nav--forward-sexp (&optional dir safe skip-parens-p)
  "Move to forward sexp.
With positive optional argument DIR direction move forward, else
backwards.  When optional argument SAFE is non-nil do not throw
errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction."
  (setq dir (or dir 1))
  (unless (= dir 0)
    (let* ((forward-p (if (> dir 0)
                          (and (setq dir 1) t)
                        (and (setq dir -1) nil)))
           (context-type (gdscript-syntax-context-type)))
      (cond
       ((memq context-type '(string comment))
        ;; Inside of a string, get out of it.
        (let ((forward-sexp-function))
          (forward-sexp dir)))
       ((and (not skip-parens-p)
             (or (eq context-type 'paren)
                 (if forward-p
                     (eq (syntax-class (syntax-after (point)))
                         (car (string-to-syntax "(")))
                   (eq (syntax-class (syntax-after (1- (point))))
                       (car (string-to-syntax ")"))))))
        ;; Inside a paren or looking at it, lisp knows what to do.
        (if safe
            (gdscript-nav--lisp-forward-sexp-safe dir)
          (gdscript-nav--lisp-forward-sexp dir)))
       (t
        ;; This part handles the lispy feel of
        ;; `gdscript-nav-forward-sexp'.  Knowing everything about the
        ;; current context and the context of the next sexp tries to
        ;; follow the lisp sexp motion commands in a symmetric manner.
        (let* ((context
                (cond
                 ((gdscript-info-beginning-of-block-p) 'block-start)
                 ((gdscript-info-end-of-block-p) 'block-end)
                 ((gdscript-info-beginning-of-statement-p) 'statement-start)
                 ((gdscript-info-end-of-statement-p) 'statement-end)))
               (next-sexp-pos
                (save-excursion
                  (if safe
                      (gdscript-nav--lisp-forward-sexp-safe dir)
                    (gdscript-nav--lisp-forward-sexp dir))
                  (point)))
               (next-sexp-context
                (save-excursion
                  (goto-char next-sexp-pos)
                  (cond
                   ((gdscript-info-beginning-of-block-p) 'block-start)
                   ((gdscript-info-end-of-block-p) 'block-end)
                   ((gdscript-info-beginning-of-statement-p) 'statement-start)
                   ((gdscript-info-end-of-statement-p) 'statement-end)
                   ((gdscript-info-statement-starts-block-p) 'starts-block)
                   ((gdscript-info-statement-ends-block-p) 'ends-block)))))
          (if forward-p
              (cond ((and (not (eobp))
                          (gdscript-info-current-line-empty-p))
                     (gdscript--util-forward-comment dir)
                     (gdscript-nav--forward-sexp dir safe skip-parens-p))
                    ((eq context 'block-start)
                     (gdscript-nav-end-of-block))
                    ((eq context 'statement-start)
                     (gdscript-nav-end-of-statement))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'ends-block))
                     (goto-char next-sexp-pos)
                     (gdscript-nav-end-of-block))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'starts-block))
                     (goto-char next-sexp-pos)
                     (gdscript-nav-end-of-block))
                    ((memq context '(statement-end block-end))
                     (goto-char next-sexp-pos)
                     (gdscript-nav-end-of-statement))
                    (t (goto-char next-sexp-pos)))
            (cond ((and (not (bobp))
                        (gdscript-info-current-line-empty-p))
                   (gdscript--util-forward-comment dir)
                   (gdscript-nav--forward-sexp dir safe skip-parens-p))
                  ((eq context 'block-end)
                   (gdscript-nav-beginning-of-block))
                  ((eq context 'statement-end)
                   (gdscript-nav-beginning-of-statement))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'starts-block))
                   (goto-char next-sexp-pos)
                   (gdscript-nav-beginning-of-block))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'ends-block))
                   (goto-char next-sexp-pos)
                   (gdscript-nav-beginning-of-block))
                  ((memq context '(statement-start block-start))
                   (goto-char next-sexp-pos)
                   (gdscript-nav-beginning-of-statement))
                  (t (goto-char next-sexp-pos))))))))))

(defun gdscript-nav-forward-sexp (&optional arg safe skip-parens-p)
  "Move forward across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times.  When optional argument SAFE is non-nil do not
throw errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction (forced to t
in interactive calls)."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; Do not follow parens on interactive calls.  This hack to detect
  ;; if the function was called interactively copes with the way
  ;; `forward-sexp' works by calling `forward-sexp-function', losing
  ;; interactive detection by checking `current-prefix-arg'.  The
  ;; reason to make this distinction is that lisp functions like
  ;; `blink-matching-open' get confused causing issues like the one in
  ;; Bug#16191.  With this approach the user gets a symmetric behavior
  ;; when working interactively while called functions expecting
  ;; paren-based sexp motion work just fine.
  (or
   skip-parens-p
   (setq skip-parens-p
         (memq real-this-command
               (list
                #'forward-sexp #'backward-sexp
                #'gdscript-nav-forward-sexp #'gdscript-nav-backward-sexp
                #'gdscript-nav-forward-sexp-safe #'gdscript-nav-backward-sexp))))
  (while (> arg 0)
    (gdscript-nav--forward-sexp 1 safe skip-parens-p)
    (setq arg (1- arg)))
  (while (< arg 0)
    (gdscript-nav--forward-sexp -1 safe skip-parens-p)
    (setq arg (1+ arg))))

(defun gdscript-nav-backward-sexp (&optional arg safe skip-parens-p)
  "Move backward across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times.  When optional argument SAFE is non-nil do not
throw errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction (forced to t
in interactive calls)."
  (interactive "^p")
  (or arg (setq arg 1))
  (gdscript-nav-forward-sexp (- arg) safe skip-parens-p))

(defun gdscript-nav-forward-sexp-safe (&optional arg skip-parens-p)
  "Move forward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times.  With optional argument SKIP-PARENS-P force
sexp motion to ignore parenthesized expressions when looking at
them in either direction (forced to t in interactive calls)."
  (interactive "^p")
  (gdscript-nav-forward-sexp arg t skip-parens-p))

(defun gdscript-nav-backward-sexp-safe (&optional arg skip-parens-p)
  "Move backward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times.  With optional argument SKIP-PARENS-P force sexp
motion to ignore parenthesized expressions when looking at them in
either direction (forced to t in interactive calls)."
  (interactive "^p")
  (gdscript-nav-backward-sexp arg t skip-parens-p))

(defun gdscript-nav--up-list (&optional dir)
  "Internal implementation of `gdscript-nav-up-list'.
DIR is always 1 or -1 and comes sanitized from
`gdscript-nav-up-list' calls."
  (let ((context (gdscript-syntax-context-type))
        (forward-p (> dir 0)))
    (cond
     ((memq context '(string comment)))
     ((eq context 'paren)
      (let ((forward-sexp-function))
        (up-list dir)))
     ((and forward-p (gdscript-info-end-of-block-p))
      (let ((parent-end-pos
             (save-excursion
               (let ((indentation (and
                                   (gdscript-nav-beginning-of-block)
                                   (current-indentation))))
                 (while (and indentation
                             (> indentation 0)
                             (>= (current-indentation) indentation)
                             (gdscript-nav-backward-block)))
                 (gdscript-nav-end-of-block)))))
        (and (> (or parent-end-pos (point)) (point))
             (goto-char parent-end-pos))))
     (forward-p (gdscript-nav-end-of-block))
     ((and (not forward-p)
           (> (current-indentation) 0)
           (gdscript-info-beginning-of-block-p))
      (let ((prev-block-pos
             (save-excursion
               (let ((indentation (current-indentation)))
                 (while (and (gdscript-nav-backward-block)
                             (>= (current-indentation) indentation))))
               (point))))
        (and (> (point) prev-block-pos)
             (goto-char prev-block-pos))))
     ((not forward-p) (gdscript-nav-beginning-of-block)))))

(defun gdscript-nav-up-list (&optional arg)
  "Move forward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move backward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (gdscript-nav--up-list 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (gdscript-nav--up-list -1)
    (setq arg (1+ arg))))

(defun gdscript-nav-backward-up-list (&optional arg)
  "Move backward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move forward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (gdscript-nav-up-list (- arg)))

(defun gdscript-nav-if-name-main ()
  "Move point at the beginning the __main__ block.
When \"if __name__ == \\='__main__\\=':\" is found returns its
position, else returns nil."
  (interactive)
  (let ((point (point))
        (found (catch 'found
                 (goto-char (point-min))
                 (while (re-search-forward
                         (gdscript-rx line-start
                                    "if" (+ space)
                                    "__name__" (+ space)
                                    "==" (+ space)
                                    (group-n 1 (or ?\" ?\'))
                                    "__main__" (backref 1) (* space) ":")
                         nil t)
                   (when (not (gdscript-syntax-context-type))
                     (beginning-of-line)
                     (throw 'found t))))))
    (if found
        (point)
      (ignore (goto-char point)))))


;;; Hideshow

(defun gdscript-hideshow-forward-sexp-function (arg)
  "Gdscript specific `forward-sexp' function for `hs-minor-mode'.
Argument ARG is ignored."
  arg  ; Shut up, byte compiler.
  (gdscript-nav-end-of-defun)
  (unless (gdscript-info-current-line-empty-p)
    (backward-char)))


;; Abbreviations
(define-abbrev-table 'gdscript-abbrev-table
  '(
    ("p" "func _process(delta):\n" )
    ("pp" "func _physics_process(delta):\n" )
    )
  "Table of abbreviations to use with abbrev-mode for `gdscript'"
  )
(abbrev-table-put gdscript-abbrev-table :regexp "\\([_-*0-9A-Za-z]+\\)")
(abbrev-table-put gdscript-abbrev-table :case-fixed t)
(abbrev-table-put gdscript-abbrev-table :system t)


(defun gdscript-electric-pair-string-delimiter ()
  (when (and electric-pair-mode
             (memq last-command-event '(?\" ?\'))
             (let ((count 0))
               (while (eq (char-before (- (point) count)) last-command-event)
                 (cl-incf count))
               (= count 3))
             (eq (char-after) last-command-event))
    (save-excursion (insert (make-string 2 last-command-event)))))

(defvar electric-indent-inhibit)
(defvar prettify-symbols-alist)

(define-derived-mode gdscript-mode prog-mode "gdscript"
  "major mode for editing gdscript files.

\\{gdscript-mode-map}"
  (setq-local tab-width gdscript-tab-width)
  (setq-local indent-tabs-mode gdscript-use-tab-indents)

  (set-syntax-table gdscript-syntax-table)
  (modify-syntax-entry ?\# "\<" gdscript-syntax-table)
  (modify-syntax-entry ?\n ">" gdscript-syntax-table)

  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-end "")
  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)

  (setq-local forward-sexp-function
              'gdscript-nav-forward-sexp)

  (setq-local font-lock-defaults
              '(gdscript-font-lock))

  (setq-local syntax-propertize-function
              gdscript-syntax-propertize-function)

  (setq-local indent-line-function
              #'gdscript-indent-line-function)
  (setq-local indent-region-function #'gdscript-indent-region)
  ;; because indentation is not redundant, we cannot safely reindent code.
  (setq-local electric-indent-inhibit t)
  (setq-local electric-indent-chars
              (cons ?: electric-indent-chars))

  ;; add """ ... """ pairing to electric-pair-mode.
  (add-hook 'post-self-insert-hook
            #'gdscript-electric-pair-string-delimiter 'append t)

  ;; (setq-local paragraph-start "\\s-*$")
  ;; (setq-local fill-paragraph-function
  ;;             #'gdscript-fill-paragraph)
  ;; (setq-local normal-auto-fill-function #'gdscript-do-auto-fill)

  (setq-local beginning-of-defun-function
              #'gdscript-nav-beginning-of-defun)
  (setq-local end-of-defun-function
              #'gdscript-nav-end-of-defun)

  (add-hook 'completion-at-point-functions
            #'gdscript-completion-at-point nil 'local)

  (add-hook 'post-self-insert-hook
            #'gdscript-indent-post-self-insert-function 'append 'local)

  (setq-local imenu-create-index-function
              #'gdscript-imenu-create-index)

  (setq-local add-log-current-defun-function
              #'gdscript-info-current-defun)

  (add-hook 'which-func-functions #'gdscript-info-current-defun nil t)

  (add-to-list
   'hs-special-modes-alist
   '(gdscript-mode
     "\\s-*\\_<\\(?:func\\|class\\)\\_>"
     ;; use the empty string as end regexp so it doesn't default to
     ;; "\\s)".  this way parens at end of defun are properly hidden.
     ""
     "#"
     gdscript-hideshow-forward-sexp-function
     nil))

  (setq-local outline-regexp
              (gdscript-rx (* space) block-start))
  (setq-local outline-heading-end-regexp ":[^\n]*\n")
  (setq-local outline-level
              #'(lambda ()
                  "`outline-level' function for gdscript mode."
                  (1+ (/ (current-indentation) gdscript-indent-offset))))

  (when gdscript-indent-guess-indent-offset
    (gdscript-indent-guess-indent-offset)))

(provide 'gdscript-mode)

;;; gdscript-mode.el ends here
