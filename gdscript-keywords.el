;;; gdscript-keywords.el --- Cached keywords for the GDScript language  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 GDQuest and contributors

;; Author: Nathan Lovato <nathan@gdquest.com>
;; Maintainer: Jen-Chieh Shen <jcs090218@gmail.com>

;; This file is not part of GNU Emacs.

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
;;
;; Cached keywords for the GDScript language, used for syntax
;; highlighting and auto-completion.
;;

;;; Code:

(defconst gdscript-keywords
  '("and" "as" "assert" "await"
    "break" "breakpoint"
    "case" "class" "class_name" "const" "continue"
    "do"
    "elif" "else" "enum"
    "export" "export_category" "export_color_no_alpha" "export_custom"
    "export_dir" "export_enum" "export_exp_easing" "export_file"
    "export_flags"
    "export_flags_2d_navigation" "export_flags_2d_physics" "export_flags_2d_render"
    "export_flags_3d_navigation" "export_flags_3d_physics" "export_flags_3d_render"
    "export_global_dir" "export_global_file" "export_group" "export_multiline"
    "export_node_path" "export_range" "export_storage" "export_subgroup" "export_tool_button"
    "extends"
    "false" "for" "func"
    "if" "in" "is"
    "master" "match"
    "not"
    "onready" "or"
    "pass" "preload" "puppet"
    "remote" "remotesync" "return"
    "self" "setget" "signal" "super" "slave" "static" "switch" "sync"
    "tool" "true"
    "var"
    "while"
    "yield"))

(defconst gdscript-built-in-constants '("INF" "NAN" "PI" "TAU"))

;; Only contains types that are not classes and that the Godot editor highlights
;; like built-in keywords
(defconst gdscript-built-in-types '("bool" "float" "int" "null" "void"))

(defconst gdscript-built-in-functions
  '("Color8" "ColorN" "abs" "absf"
    "acos" "acosh" "angle_difference" "asin" "asinh" "atan" "atan2" "atanh"
    "bezier_derivative" "bezier_interpolate" "bytes2var" "bytes_to_var" "bytes_to_var_with_objects"
    "cartesian2polar" "ceil" "ceilf" "ceili" "char" "clamp" "convert"
    "cos" "cosh" "cubic_interpolate" "cubic_interpolate_angle"
    "cubic_interpolate_angle_in_time" "cubic_interpolate_in_time"
    "db2linear" "db_to_linear" "decimals" "dectime" "deg2rad" "deg_to_rad"
    "dict2inst" "dict_to_inst"
    "ease" "error_string" "exp"
    "floor" "floorf" "floori" "fmod" "fposmod" "funcref"
    "get_stack"
    "hash"
    "inst2dict" "inst_to_dict" "instance_from_id" "inverse_lerp"
    "is_equal_approx" "is_finite" "is_inf" "is_instance_id_valid" "is_instance_valid"
    "is_nan" "is_same" "is_zero_approx"
    "len" "lerp" "lerp_angle" "lerpf" "linear2db" "linear_to_db" "load" "log"
    "max" "maxf" "maxi" "min" "minf" "mini" "move_toward"
    "nearest_po2"
    "ord"
    "parse_json" "pingpong" "polar2cartesian" "posmod" "pow" "preload"
    "print" "print_debug" "print_rich" "print_stack" "print_verbose" 
    "printerr" "printraw" "prints" "printt"
    "push_error" "push_warning"
    "rad2deg" "rad_to_deg" "rand_from_seed" "rand_range" "rand_seed"
    "randf" "randf_range" "randfn" "randi" "randi_range"
    "randomize" "range" "range_lerp" "remap" "rid_allocate_id" "rid_from_int64"
    "round" "roundf" "roundi"
    "seed" "sign" "signf" "signi" "sin" "sinh" "smoothstep" "sqrt" "step_decimals" "stepify"
    "str" "str2var" "str_to_var"
    "tan" "tanh" "to_json" "type_convert" "type_exists" "type_string" "typeof"
    "validate_json" "var2bytes" "var_to_bytes" "var_to_bytes_with_objects"
    "var2str" "var_to_str"
    "weakref" "wrap" "wrapf" "wrapi"))

;; Contains all engine classes and node types, including vectors, transforms, etc.
(defconst gdscript-built-in-classes
  '("AABB" "ARVRAnchor" "ARVRCamera" "ARVRController"
    "ARVRInterface" "ARVROrigin" "ARVRPositionalTracker" "ARVRServer" "AStar" "AStar2D" "AcceptDialog"
    "AnimatedSprite" "AnimatedSprite3D" "AnimatedTexture" "Animation" "AnimationNode"
    "AnimationNodeAdd2" "AnimationNodeAdd3" "AnimationNodeAnimation" "AnimationNodeBlend2"
    "AnimationNodeBlend3" "AnimationNodeBlendSpace1D" "AnimationNodeBlendSpace2D"
    "AnimationNodeBlendTree" "AnimationNodeOneShot" "AnimationNodeOutput" "AnimationNodeStateMachine"
    "AnimationNodeStateMachinePlayback" "AnimationNodeStateMachineTransition" "AnimationNodeTimeScale"
    "AnimationNodeTimeSeek" "AnimationNodeTransition" "AnimationPlayer" "AnimationRootNode"
    "AnimationTrackEditPlugin" "AnimationTree" "AnimationTreePlayer" "Area" "Area2D" "Array"
    "ArrayMesh" "AtlasTexture" "AudioBusLayout" "AudioEffect" "AudioEffectAmplify"
    "AudioEffectBandLimitFilter" "AudioEffectBandPassFilter" "AudioEffectChorus"
    "AudioEffectCompressor" "AudioEffectDelay" "AudioEffectDistortion" "AudioEffectEQ"
    "AudioEffectEQ10" "AudioEffectEQ21" "AudioEffectEQ6" "AudioEffectFilter"
    "AudioEffectHighPassFilter" "AudioEffectHighShelfFilter" "AudioEffectInstance"
    "AudioEffectLimiter" "AudioEffectLowPassFilter" "AudioEffectLowShelfFilter"
    "AudioEffectNotchFilter" "AudioEffectPanner" "AudioEffectPhaser" "AudioEffectPitchShift"
    "AudioEffectRecord" "AudioEffectReverb" "AudioEffectSpectrumAnalyzer"
    "AudioEffectSpectrumAnalyzerInstance" "AudioEffectStereoEnhance" "AudioServer" "AudioStream"
    "AudioStreamGenerator" "AudioStreamGeneratorPlayback" "AudioStreamMicrophone"
    "AudioStreamPlayback" "AudioStreamPlaybackResampled" "AudioStreamPlayer" "AudioStreamPlayer2D"
    "AudioStreamPlayer3D" "AudioStreamRandomPitch" "AudioStreamSample" "BackBufferCopy"
    "BakedLightmap" "BakedLightmapData" "BaseButton" "Basis" "BitMap" "BitmapFont" "Bone2D"
    "BoneAttachment" "BoxContainer" "BoxShape" "Button" "ButtonGroup" "CPUParticles" "CPUParticles2D"
    "Camera" "Camera2D" "CameraFeed" "CameraServer" "CameraTexture" "CanvasItem" "CanvasItemMaterial"
    "CanvasLayer" "CanvasModulate" "CapsuleMesh" "CapsuleShape" "CapsuleShape2D" "CenterContainer"
    "CharFXTransform" "CheckBox" "CheckButton" "CircleShape2D" "ClassDB" "ClippedCamera"
    "CollisionObject" "CollisionObject2D" "CollisionPolygon" "CollisionPolygon2D" "CollisionShape"
    "CollisionShape2D" "Color" "ColorPicker" "ColorPickerButton" "ColorRect" "ConcavePolygonShape"
    "ConcavePolygonShape2D" "ConeTwistJoint" "ConfigFile" "ConfirmationDialog" "Container" "Control"
    "ConvexPolygonShape" "ConvexPolygonShape2D" "Crypto" "CryptoKey" "CubeMap" "CubeMesh" "Curve"
    "Curve2D" "Curve3D" "CurveTexture" "CylinderMesh" "CylinderShape" "DampedSpringJoint2D"
    "Dictionary" "DirectionalLight" "Directory" "DynamicFont" "DynamicFontData" "EditorExportPlugin"
    "EditorFeatureProfile" "EditorFileDialog" "EditorFileSystem" "EditorFileSystemDirectory"
    "EditorImportPlugin" "EditorInspector" "EditorInspectorPlugin" "EditorInterface"
    "EditorNavigationMeshGenerator" "EditorPlugin" "EditorProperty" "EditorResourceConversionPlugin"
    "EditorResourcePreview" "EditorResourcePreviewGenerator" "EditorSceneImporter"
    "EditorSceneImporterAssimp" "EditorScenePostImport" "EditorScript" "EditorSelection"
    "EditorSettings" "EditorSpatialGizmo" "EditorSpatialGizmoPlugin" "EditorSpinSlider"
    "EditorVCSInterface" "EncodedObjectAsID" "Engine" "Environment" "Expression" "File" "FileDialog"
    "Font" "FuncRef" "GIProbe" "GIProbeData" "Generic6DOFJoint" "Geometry" "GeometryInstance"
    "Gradient" "GradientTexture" "GraphEdit" "GraphNode" "GridContainer" "GrooveJoint2D"
    "HBoxContainer" "HScrollBar" "HSeparator" "HSlider" "HSplitContainer" "HTTPClient" "HTTPRequest"
    "HashingContext" "HeightMapShape" "HingeJoint" "IP" "IP_Unix" "Image" "ImageTexture"
    "ImmediateGeometry" "Input" "InputDefault" "InputEvent" "InputEventAction" "InputEventGesture"
    "InputEventJoypadButton" "InputEventJoypadMotion" "InputEventKey" "InputEventMIDI"
    "InputEventMagnifyGesture" "InputEventMouse" "InputEventMouseButton" "InputEventMouseMotion"
    "InputEventPanGesture" "InputEventScreenDrag" "InputEventScreenTouch" "InputEventWithModifiers"
    "InputMap" "InstancePlaceholder" "InterpolatedCamera" "ItemList" "JSON" "JSONParseResult"
    "JSONRPC" "JavaScript" "Joint" "Joint2D" "KinematicBody" "KinematicBody2D" "KinematicCollision"
    "KinematicCollision2D" "Label" "LargeTexture" "Light" "Light2D" "LightOccluder2D" "Line2D"
    "LineEdit" "LineShape2D" "LinkButton" "Listener" "MainLoop" "MarginContainer" "Marshalls"
    "Material" "MenuButton" "Mesh" "MeshDataTool" "MeshInstance" "MeshInstance2D" "MeshLibrary"
    "MeshTexture" "MultiMesh" "MultiMeshInstance" "MultiMeshInstance2D" "MultiplayerAPI" "Mutex"
    "Navigation" "Navigation2D" "NavigationMesh" "NavigationMeshInstance" "NavigationPolygon"
    "NavigationPolygonInstance" "NetworkedMultiplayerPeer" "Nil" "NinePatchRect" "Node" "Node2D"
    "NodePath" "OS" "Object" "OccluderPolygon2D" "OmniLight" "OptionButton" "PCKPacker"
    "PHashTranslation" "PackedDataContainer" "PackedDataContainerRef" "PackedScene" "PacketPeer"
    "PacketPeerStream" "PacketPeerUDP" "Panel" "PanelContainer" "PanoramaSky" "ParallaxBackground"
    "ParallaxLayer" "Particles" "Particles2D" "ParticlesMaterial" "Path" "Path2D" "PathFollow"
    "PathFollow2D" "Performance" "PhysicalBone" "Physics2DDirectBodyState"
    "Physics2DDirectBodyStateSW" "Physics2DDirectSpaceState" "Physics2DServer" "Physics2DServerSW"
    "Physics2DShapeQueryParameters" "Physics2DShapeQueryResult" "Physics2DTestMotionResult"
    "PhysicsBody" "PhysicsBody2D" "PhysicsDirectBodyState" "PhysicsDirectSpaceState" "PhysicsMaterial"
    "PhysicsServer" "PhysicsShapeQueryParameters" "PhysicsShapeQueryResult" "PinJoint" "PinJoint2D"
    "Plane" "PlaneMesh" "PlaneShape" "PointMesh" "Polygon2D" "PolygonPathFinder" "PoolByteArray"
    "PoolColorArray" "PoolIntArray" "PoolRealArray" "PoolStringArray" "PoolVector2Array"
    "PoolVector3Array" "Popup" "PopupDialog" "PopupMenu" "PopupPanel" "Position2D" "Position3D"
    "PrimitiveMesh" "PrismMesh" "ProceduralSky" "ProgressBar" "ProjectSettings" "ProximityGroup"
    "ProxyTexture" "QuadMesh" "Quat" "RID" "RandomNumberGenerator" "Range" "RayCast" "RayCast2D"
    "RayShape" "RayShape2D" "Rect2" "RectangleShape2D" "Reference" "ReferenceRect" "ReflectionProbe"
    "RemoteTransform" "RemoteTransform2D" "Resource" "ResourceFormatLoader"
    "ResourceFormatLoaderCrypto" "ResourceFormatSaver" "ResourceFormatSaverCrypto" "ResourceImporter"
    "ResourceInteractiveLoader" "ResourceLoader" "ResourcePreloader" "ResourceSaver" "RichTextEffect"
    "RichTextLabel" "RigidBody" "RigidBody2D" "RootMotionView" "SceneState" "SceneTree"
    "SceneTreeTimer" "Script" "ScriptCreateDialog" "ScriptEditor" "ScrollBar" "ScrollContainer"
    "SegmentShape2D" "Semaphore" "Separator" "Shader" "ShaderMaterial" "Shape" "Shape2D" "ShortCut"
    "Skeleton" "Skeleton2D" "SkeletonIK" "Skin" "SkinReference" "Sky" "Slider" "SliderJoint"
    "SoftBody" "Spatial" "SpatialGizmo" "SpatialMaterial" "SpatialVelocityTracker" "SphereMesh"
    "SphereShape" "SpinBox" "SplitContainer" "SpotLight" "SpringArm" "Sprite" "Sprite3D"
    "SpriteBase3D" "SpriteFrames" "StaticBody" "StaticBody2D" "StreamPeer" "StreamPeerBuffer"
    "StreamPeerSSL" "StreamPeerTCP" "StreamTexture" "String" "StyleBox" "StyleBoxEmpty" "StyleBoxFlat"
    "StyleBoxLine" "StyleBoxTexture" "SurfaceTool" "TCP_Server" "TabContainer" "Tabs" "TextEdit"
    "TextFile" "Texture" "Texture3D" "TextureArray" "TextureButton" "TextureLayered" "TextureProgress"
    "TextureRect" "Theme" "Thread" "TileMap" "TileSet" "Timer" "ToolButton" "TouchScreenButton"
    "Transform" "Transform2D" "Translation" "TranslationServer" "Tree" "TreeItem" "TriangleMesh"
    "Tween" "UndoRedo" "VBoxContainer" "VScrollBar" "VSeparator" "VSlider" "VSplitContainer" "Variant"
    "Vector2" "Vector3" "VehicleBody" "VehicleWheel" "VideoPlayer" "VideoStream" "Viewport"
    "ViewportContainer" "ViewportTexture" "VisibilityEnabler" "VisibilityEnabler2D"
    "VisibilityNotifier" "VisibilityNotifier2D" "VisualInstance" "VisualServer" "VisualShader"
    "VisualShaderNode" "VisualShaderNodeBooleanConstant" "VisualShaderNodeBooleanUniform"
    "VisualShaderNodeColorConstant" "VisualShaderNodeColorFunc" "VisualShaderNodeColorOp"
    "VisualShaderNodeColorUniform" "VisualShaderNodeCompare" "VisualShaderNodeCubeMap"
    "VisualShaderNodeCubeMapUniform" "VisualShaderNodeCustom" "VisualShaderNodeDeterminant"
    "VisualShaderNodeDotProduct" "VisualShaderNodeExpression" "VisualShaderNodeFaceForward"
    "VisualShaderNodeFresnel" "VisualShaderNodeGlobalExpression" "VisualShaderNodeGroupBase"
    "VisualShaderNodeIf" "VisualShaderNodeInput" "VisualShaderNodeIs" "VisualShaderNodeOuterProduct"
    "VisualShaderNodeOutput" "VisualShaderNodeScalarClamp" "VisualShaderNodeScalarConstant"
    "VisualShaderNodeScalarDerivativeFunc" "VisualShaderNodeScalarFunc" "VisualShaderNodeScalarInterp"
    "VisualShaderNodeScalarOp" "VisualShaderNodeScalarSmoothStep" "VisualShaderNodeScalarSwitch"
    "VisualShaderNodeScalarUniform" "VisualShaderNodeSwitch" "VisualShaderNodeTexture"
    "VisualShaderNodeTextureUniform" "VisualShaderNodeTextureUniformTriplanar"
    "VisualShaderNodeTransformCompose" "VisualShaderNodeTransformConstant"
    "VisualShaderNodeTransformDecompose" "VisualShaderNodeTransformFunc"
    "VisualShaderNodeTransformMult" "VisualShaderNodeTransformUniform"
    "VisualShaderNodeTransformVecMult" "VisualShaderNodeUniform" "VisualShaderNodeVec3Constant"
    "VisualShaderNodeVec3Uniform" "VisualShaderNodeVectorClamp" "VisualShaderNodeVectorCompose"
    "VisualShaderNodeVectorDecompose" "VisualShaderNodeVectorDerivativeFunc"
    "VisualShaderNodeVectorDistance" "VisualShaderNodeVectorFunc" "VisualShaderNodeVectorInterp"
    "VisualShaderNodeVectorLen" "VisualShaderNodeVectorOp" "VisualShaderNodeVectorRefract"
    "VisualShaderNodeVectorScalarMix" "VisualShaderNodeVectorScalarSmoothStep"
    "VisualShaderNodeVectorScalarStep" "VisualShaderNodeVectorSmoothStep" "WeakRef" "WindowDialog"
    "World" "World2D" "WorldEnvironment" "X509Certificate" "XMLParser" "YSort"))

(provide 'gdscript-keywords)
;;; gdscript-keywords.el ends here
