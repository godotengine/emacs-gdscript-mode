;;; gdscript-mode.el --- Major mode to edit Godot GDScript code.
;; URL: https://github.com/GDQuest/emacs-gdscript-mode
;; Version: 202001141-git

(defvar gdscript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap newline-and-indent] 'gdscript-newline-and-indent) map))

;; user customization
(defcustom gdscript-tabs-mode t
  "Use tabs (t) or spaces (nil)"
  :type 'boolean
  :group 'gdscript)

(defcustom gdscript-tab-width 4
  "Indentation width"
  :type 'integer
  :group 'gdscript)


;; Lists of keywords in the language
(defvar gdscript-keywords '("if" "elif" "else" "for" "do" "while" "match"
                            "switch" "case" "break" "continue" "pass"
                            "return" "class" "extends" "is" "self" "tool"
                            "signal" "func" "static" "const" "enum" "var"
                            "onready" "export" "setget" "breakpoint" "preload"
                            "yield" "assert" "remote" "master" "slave"
                            "sync"))

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

(defvar gdscript-syntax-table nil)
(if gdscript-syntax-table
    ()
  (setq gdscript-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\# "\<" gdscript-syntax-table)
  (modify-syntax-entry ?\n ">" gdscript-syntax-table))



;;Indentation
(defun gdscript-should-indent ()
  (save-excursion
    (skip-chars-backward "\r\n\t ");;maybe don't needs this
    (let ((char-eol (char-before (line-end-position))))
      (char-equal ?\: char-eol))))

(defun gdscript-max-indent ()
  (save-excursion
    (skip-chars-backward "\r\n\t ")
    (if (gdscript-should-indent)
        (+ (current-indentation) gdscript-tab-width)
      (current-indentation))))

(defun gdscript-insert-tab (c)
  (if gdscript-tabs-mode
      (insert-char (string-to-char "\t") (floor c gdscript-tab-width))
    (insert-char ?  c)))

(defun gdscript-newline-and-indent ()
  (interactive)
  (delete-horizontal-space t)
  (newline)
  (gdscript-insert-tab (gdscript-max-indent)))

(defun gdscript-indent-back (c)
  (delete-horizontal-space)
  (gdscript-insert-tab (- c gdscript-tab-width)))

(defun is-blank-line? ()
  (= (count-words (line-beginning-position) (line-end-position)) 0))

;;I think I need a custom function to determine current indentation
;;also need to find a better way to do this stuff as if it's a blank line it behaves strange
;;when using save-excursion right now this looks really dumb
(defun gdscript-indent-line ()
  (interactive)
  (let ((ci (current-indentation))
        (co (current-column)))
    (back-to-indentation)
    (cond ((and (<= ci (gdscript-max-indent)) (> ci 0))
           (if (is-blank-line?)
               (gdscript-indent-back ci)
             (progn
               (move-to-column co)
               (save-excursion
                 (back-to-indentation)
                 (gdscript-indent-back ci)))))
          ((= ci 0)
           (if (is-blank-line?)
               (gdscript-insert-tab (gdscript-max-indent))
             (progn
               (move-to-column co)
               (save-excursion
                 (back-to-indentation)
                 (gdscript-insert-tab (gdscript-max-indent))))))
          )
    ))


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


(define-derived-mode gdscript-mode
  prog-mode
  "GDScript"
  :abbrev-table gdscript-abbrev-table
  (setq-local indent-line-function 'gdscript-indent-line)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (set-syntax-table gdscript-syntax-table)
  (setq-local font-lock-defaults
              '(gdscript-font-lock))
  (abbrev-mode 1))

(provide 'gdscript-mode)

(add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-mode))

;;; gdscript-mode.el ends here
